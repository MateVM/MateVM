{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Mate.X86TrapHandling (
  mateHandler,
  register_signal
  ) where

import Numeric
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B

import Foreign
import Foreign.C.Types

import Harpy

import Mate.Types
import Mate.NativeSizes
import {-# SOURCE #-} Mate.MethodPool
import Mate.ClassPool
import Mate.X86CodeGen

import Mate.Debug
import Harpy.X86Disassembler

foreign import ccall "register_signal"
  register_signal :: IO ()

foreign export ccall mateHandler :: CPtrdiff -> CPtrdiff -> CPtrdiff -> CPtrdiff -> IO CPtrdiff
mateHandler :: CPtrdiff -> CPtrdiff -> CPtrdiff -> CPtrdiff -> IO CPtrdiff
mateHandler reip reax rebx resi = do
  tmap <- getTrapMap
  case M.lookup (fromIntegral reip) tmap of
    (Just (StaticMethod _)) -> patchWithHarpy patchStaticCall reip
    (Just (StaticField _))  -> staticFieldHandler reip
    (Just (InstanceOf cn))  -> patchWithHarpy (`patchInstanceOf` cn) reip
    (Just (NewObject cn))   -> patchWithHarpy (`patchNewObject` cn) reip
    (Just (VirtualCall False _ io_offset)) ->
          patchWithHarpy (patchInvoke reax reax io_offset) reip
    (Just (VirtualCall True  _ io_offset)) ->
          patchWithHarpy (patchInvoke rebx reax io_offset) reip
    Nothing -> case resi of
        0x13371234 -> return (-1)
        _ -> error $ "getTrapType: abort :-( " ++ (showHex reip ". ")
             ++ (concatMap (`showHex` ", ") (M.keys tmap))

patchWithHarpy :: (CPtrdiff -> CodeGen () () CPtrdiff) -> CPtrdiff -> IO CPtrdiff
patchWithHarpy patcher reip = do
  -- this is just an upperbound. if the value is to low, patching fails. find
  -- something better?
  let fixme = 1024
  let entry = Just (intPtrToPtr (fromIntegral reip), fixme)
  let cgconfig = defaultCodeGenConfig { customCodeBuffer = entry }
  (_, Right right) <- runCodeGenWithConfig (withDisasm $ patcher reip) () () cgconfig
  mapM_ (printfJit . printf "patched: %s\n" . showAtt) $ snd right
  return reip

withDisasm :: CodeGen e s CPtrdiff -> CodeGen e s (CPtrdiff, [Instruction])
withDisasm patcher = do
  reip <- patcher
  d <- disassemble
  return (reip, d)

patchStaticCall :: CPtrdiff -> CodeGen e s CPtrdiff
patchStaticCall reip = do
  entryAddr <- liftIO $ getMethodEntry reip 0
  call (fromIntegral (entryAddr - (reip + 5)) :: NativeWord)
  return reip


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

patchInstanceOf :: CPtrdiff -> B.ByteString -> CodeGen e s CPtrdiff
patchInstanceOf reip classname = do
  mtable <- liftIO $ getMethodTable classname
  mov edx mtable
  return reip

patchNewObject :: CPtrdiff -> B.ByteString -> CodeGen e s CPtrdiff
patchNewObject reip classname = do
  objsize <- liftIO $ getObjectSize classname
  push32 objsize
  callMalloc
  mtable <- liftIO $ getMethodTable classname
  mov (Disp 0, eax) mtable
  return reip

patchInvoke :: CPtrdiff -> CPtrdiff -> IO NativeWord -> CPtrdiff -> CodeGen e s CPtrdiff
patchInvoke method_table table2patch io_offset reip = do
  offset <- liftIO io_offset
  entryAddr <- liftIO $ getMethodEntry reip method_table
  call32_eax (Disp offset)
  -- patch entry in table
  let call_insn = intPtrToPtr . fromIntegral $ table2patch + fromIntegral offset
  liftIO $ poke call_insn entryAddr
  return reip
