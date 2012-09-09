{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Mate.X86TrapHandling (
  mateHandler,
  register_signal
  ) where

import Numeric
import qualified Data.Map as M
import Control.Monad

import Foreign
import Foreign.C.Types

import Harpy hiding (fst)

import Mate.Types
import Mate.NativeSizes
import {-# SOURCE #-} Mate.MethodPool
import Mate.ClassPool
import Mate.X86CodeGen

import Mate.Debug
import Harpy.X86Disassembler

foreign import ccall "register_signal"
  register_signal :: IO ()

foreign export ccall mateHandler :: CPtrdiff -> CPtrdiff -> CPtrdiff -> CPtrdiff -> CPtrdiff -> IO CPtrdiff
mateHandler :: CPtrdiff -> CPtrdiff -> CPtrdiff -> CPtrdiff -> CPtrdiff -> IO CPtrdiff
mateHandler reip reax rebx resi resp = do
  tmap <- getTrapMap
  let reipw32 = fromIntegral reip
  (deleteMe, ret_nreip) <- case M.lookup reipw32 tmap of
    (Just (StaticMethod patcher)) ->
        patchWithHarpy patcher reip >>= delFalse
    (Just (StaticField _))  ->
        staticFieldHandler reip >>= delTrue
    (Just (ObjectField patcher)) ->
        patchWithHarpy patcher reip >>= delTrue
    (Just (InstanceOf patcher))  ->
        patchWithHarpy (patcher reax) reip >>= delFalse
    (Just (ThrowException patcher)) ->
        patchWithHarpy (patcher reax resp) reip >>= delFalse
    (Just (NewObject patcher))   ->
        patchWithHarpy patcher reip >>= delTrue
    (Just (VirtualCall False mi io_offset)) ->
        patchWithHarpy (patchInvoke mi reax reax io_offset) reip
        >>= delFalse
    (Just (VirtualCall True  mi io_offset)) ->
        patchWithHarpy (patchInvoke mi rebx reax io_offset) reip
        >>= delFalse
    Nothing -> case resi of
        0x13371234 -> delFalse (-1)
        _ -> error $ "getTrapType: abort :-( eip: "
             ++ showHex reip ". " ++ concatMap (`showHex` ", ") (M.keys tmap)
  when deleteMe $ setTrapMap $ M.delete reipw32 tmap
  return ret_nreip
    where
      delTrue x = return (True,x)
      delFalse x = return (False,x)


patchWithHarpy :: (CPtrdiff -> CodeGen () () CPtrdiff) -> CPtrdiff -> IO CPtrdiff
patchWithHarpy patcher reip = do
  -- this is just an upperbound. if the value is to low, patching fails. find
  -- something better?
  let fixme = 1024
  let entry = Just (intPtrToPtr (fromIntegral reip), fixme)
  let cgconfig = defaultCodeGenConfig { customCodeBuffer = entry }
  (_, Right right) <- runCodeGenWithConfig (withDisasm $ patcher reip) () () cgconfig
  when mateDEBUG $ mapM_ (printfJit . printf "patched: %s\n" . showIntel) $ snd right
  return $ fst right

withDisasm :: CodeGen e s CPtrdiff -> CodeGen e s (CPtrdiff, [Instruction])
withDisasm patcher = do
  reip <- patcher
  d <- disassemble
  return (reip, d)

staticFieldHandler :: CPtrdiff -> IO CPtrdiff
staticFieldHandler reip = do
  -- patch the offset here, first two bytes are part of the insn (opcode + reg)
  let imm_ptr = intPtrToPtr (fromIntegral (reip + 2)) :: Ptr CPtrdiff
  checkMe <- peek imm_ptr
  if checkMe == 0x00000000 then
    do
      getStaticFieldAddr reip >>= poke imm_ptr
      return reip
    else error "staticFieldHandler: something is wrong here. abort.\n"

patchInvoke :: MethodInfo -> CPtrdiff -> CPtrdiff -> IO NativeWord -> CPtrdiff -> CodeGen e s CPtrdiff
patchInvoke (MethodInfo methname _ msig)  method_table table2patch io_offset reip = do
  vmap <- liftIO getVirtualMap
  let newmi = MethodInfo methname (vmap M.! fromIntegral method_table) msig
  offset <- liftIO io_offset
  (entryAddr, _) <- liftIO $ getMethodEntry newmi
  call32Eax (Disp offset)
  -- patch entry in table
  let call_insn = intPtrToPtr . fromIntegral $ table2patch + fromIntegral offset
  liftIO $ poke call_insn entryAddr
  return reip
