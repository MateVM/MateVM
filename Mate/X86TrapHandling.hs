{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Mate.X86TrapHandling (
  mateHandler,
  register_signal
  ) where

import qualified Data.Map as M
import Control.Monad

import Foreign
import Foreign.C.Types

import Harpy hiding (fst)

import Mate.Types
import Mate.JavaObjects
import Mate.NativeSizes
import {-# SOURCE #-} Mate.MethodPool
import Mate.ClassPool
import Mate.X86CodeGen

import Mate.Debug
import Harpy.X86Disassembler

foreign import ccall "register_signal"
  register_signal :: IO ()

type MateHandlerType = CPtrdiff -> CPtrdiff -> CPtrdiff ->
                       CPtrdiff -> CPtrdiff -> CPtrdiff ->
                       CUIntPtr -> IO ()
foreign export ccall mateHandler :: MateHandlerType
mateHandler :: MateHandlerType
mateHandler reip reax rebx resi rebp resp retarr = do
  tmap <- getTrapMap
  let reipw32 = fromIntegral reip
  let wbr = WriteBackRegs { wbEip = reip, wbEbp = rebp, wbEsp = resp, wbEax = reax }
  (deleteMe, ret_wbr) <- case M.lookup reipw32 tmap of
    (Just (StaticMethod patcher)) ->
        patchWithHarpy patcher wbr >>= delFalse
    (Just (StaticField _)) ->
        staticFieldHandler wbr >>= delTrue
    (Just (ObjectField patcher)) ->
        patchWithHarpy patcher wbr >>= delTrue
    (Just (InstanceOf patcher)) ->
        patchWithHarpy patcher wbr >>= delFalse
    (Just (ThrowException patcher)) ->
        patchWithHarpy patcher wbr >>= delFalse
    (Just (NewObject patcher)) ->
        patchWithHarpy patcher wbr >>= delTrue
    (Just (VirtualCall False mi io_offset)) ->
        patchWithHarpy (patchInvoke mi reax reax io_offset) wbr >>= delFalse
    (Just (VirtualCall True  mi io_offset)) ->
        patchWithHarpy (patchInvoke mi rebx reax io_offset) wbr >>= delFalse
    Nothing -> do
      -- TODO(bernhard) check if it was segfault
      ex <- allocAndInitObject "java/lang/NullPointerException"
      -- push exception ref on the stack
      let lesp = wbEsp wbr - 4
      poke (intPtrToPtr . fromIntegral $ lesp) ex
      handleExceptionPatcher (wbr { wbEax = ex, wbEsp = lesp}) >>= delFalse
      {-
      _ -> error $ "getTrapType: abort :-( eip: "
           ++ showHex reip ". " ++ concatMap (`showHex` ", ") (M.keys tmap)
      -}
  when deleteMe $ setTrapMap $ M.delete reipw32 tmap
  pokeReg 0x0 wbEip ret_wbr
  pokeReg 0x4 wbEbp ret_wbr
  pokeReg 0x8 wbEsp ret_wbr
  pokeReg 0xc wbEax ret_wbr
    where
      delTrue x = return (True,x)
      delFalse x = return (False,x)
      addr = intPtrToPtr . fromIntegral $ retarr
      pokeReg off cons ret_wbr = poke (plusPtr addr off) (fromIntegral (cons ret_wbr) :: Word32)


patchWithHarpy :: TrapPatcher -> WriteBackRegs -> IO WriteBackRegs
patchWithHarpy patcher wbr = do
  -- this is just an upperbound. if the value is to low, patching fails. find
  -- something better?
  let fixme = 1024
  let entry = Just (intPtrToPtr (fromIntegral $ wbEip wbr), fixme)
  let cgconfig = defaultCodeGenConfig { customCodeBuffer = entry }
  (_, Right right) <- runCodeGenWithConfig (withDisasm $ patcher wbr) () () cgconfig
  when mateDEBUG $ mapM_ (printfJit . printf "patched: %s\n" . showIntel) $ snd right
  return $ fst right

withDisasm :: CodeGen e s WriteBackRegs -> CodeGen e s (WriteBackRegs, [Instruction])
withDisasm patcher = do
  rval <- patcher
  d <- disassemble
  return (rval, d)

staticFieldHandler :: WriteBackRegs -> IO WriteBackRegs
staticFieldHandler wbr = do
  -- patch the offset here, first two bytes are part of the insn (opcode + reg)
  let imm_ptr = intPtrToPtr (fromIntegral (wbEip wbr + 2)) :: Ptr CPtrdiff
  checkMe <- peek imm_ptr
  if checkMe == 0x00000000 then
    do
      getStaticFieldAddr (wbEip wbr) >>= poke imm_ptr
      return wbr
    else error "staticFieldHandler: something is wrong here. abort.\n"

patchInvoke :: MethodInfo -> CPtrdiff -> CPtrdiff -> IO NativeWord ->
               WriteBackRegs -> CodeGen e s WriteBackRegs
patchInvoke (MethodInfo methname _ msig)  method_table table2patch io_offset wbr = do
  vmap <- liftIO getVirtualMap
  liftIO $ printfJit $ printf "patched virtual call: issued from 0x%08x\n" (fromIntegral (wbEip wbr) :: Word32)
  when (method_table == 0) $ error "patchInvoke: method_table is null.  abort."
  let newmi = MethodInfo methname (vmap M.! fromIntegral method_table) msig
  offset <- liftIO io_offset
  (entryAddr, _) <- liftIO $ getMethodEntry newmi
  call32Eax (Disp offset)
  -- patch entry in table
  let call_insn = intPtrToPtr . fromIntegral $ table2patch + fromIntegral offset
  liftIO $ poke call_insn entryAddr
  liftIO $ printfJit $ printf "patched virtual call: 0x%08x\n" (fromIntegral entryAddr :: Word32)
  return wbr
