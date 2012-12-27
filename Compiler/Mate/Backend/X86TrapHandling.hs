{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Compiler.Mate.Backend.X86TrapHandling (
  mateHandler,
  register_signal
  ) where

import qualified Data.Map as M
import Control.Monad

import Foreign
import Foreign.C.Types

import JVM.ClassFile

import Harpy hiding (fst)

import Compiler.Mate.Types
import Compiler.Mate.Backend.NativeSizes
import Compiler.Mate.Runtime.JavaObjects
import {-# SOURCE #-} Compiler.Mate.Runtime.MethodPool
import Compiler.Mate.Runtime.ClassPool
import Compiler.Mate.Backend.X86CodeGenerator

import Compiler.Mate.Debug
import Numeric

import Harpy.X86Disassembler

foreign import ccall "register_signal"
  register_signal :: IO ()

type MateHandlerType = CPtrdiff -> CPtrdiff -> CPtrdiff ->
                       CPtrdiff -> CPtrdiff -> CPtrdiff ->
                       CPtrdiff -> CUIntPtr -> IO ()
foreign export ccall mateHandler :: MateHandlerType
mateHandler :: MateHandlerType
mateHandler reip reax rebx resi rebp resp recx retarr = do
  tmap <- getTrapMap
  printfTrap "----------------------\nenter matehandler\n"
  let reipw32 = fromIntegral reip
  let wbr = M.fromList [(eip, reip), (ebp, rebp), (esp, resp), (eax, reax)]
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
    (Just (VirtualCall True  mi io_offset)) -> -- interface call
        patchWithHarpy (patchInvoke mi rebx reax io_offset) wbr >>= delFalse
    Nothing -> do
      -- TODO(bernhard) check if it was segfault
      ex <- allocAndInitObject "java/lang/NullPointerException"
      printfTrap $ "getTrapType: abort :-( eip: "
           ++ showHex (fromIntegral reip :: Word32) ".   "
           ++ concatMap (`showHex` ", ") (M.keys tmap)
           ++ printf "\neax: 0x%08x" (fromIntegral reax :: Word32)
           ++ printf "\nebx: 0x%08x" (fromIntegral rebx :: Word32)
           ++ printf "\necx: 0x%08x" (fromIntegral recx :: Word32)
           ++ printf "\nesi: 0x%08x" (fromIntegral resi :: Word32)
           ++ printf "\nebp: 0x%08x" (fromIntegral rebp :: Word32)
      -- push exception ref on the stack
      let lesp = (wbr M.! esp) - 4
      poke (intPtrToPtr . fromIntegral $ lesp) ex
      handleExceptionPatcher (M.insert eax ex $ M.insert esp lesp wbr) >>= delFalse
  when deleteMe $ setTrapMap $ M.delete reipw32 tmap
  pokeReg 0x0 eip ret_wbr
  pokeReg 0x4 ebp ret_wbr
  pokeReg 0x8 esp ret_wbr
  pokeReg 0xc eax ret_wbr
  printfTrap "nothing todo here *fly away*\n"
    where
      delTrue x = return (True,x)
      delFalse x = return (False,x)
      addr = intPtrToPtr . fromIntegral $ retarr
      pokeReg off cons ret_wbr = poke (plusPtr addr off) (fromIntegral (ret_wbr M.! cons) :: Word32)


patchWithHarpy :: TrapPatcher -> WriteBackRegs -> IO WriteBackRegs
patchWithHarpy patcher wbr = do
  -- this is just an upperbound. if the value is to low, patching fails. find
  -- something better?
  let fixme = 1024
  let entry = Just (intPtrToPtr (fromIntegral (wbr M.! eip)), fixme)
  let cgconfig = defaultCodeGenConfig { customCodeBuffer = entry }
  printfTrap "try patching with harpy now\n"
  (_, Right right) <- runCodeGenWithConfig (withDisasm $ patcher wbr) () () cgconfig
  when mateDEBUG $ mapM_ (printfTrap . printf "patched: %s\n" . showIntel) $ snd right
  return $ fst right

withDisasm :: CodeGen e s WriteBackRegs -> CodeGen e s (WriteBackRegs, [Instruction])
withDisasm patcher = do
  rval <- patcher
  d <- if mateDEBUG
    then disassemble
    else return []
  return (rval, d)

staticFieldHandler :: WriteBackRegs -> IO WriteBackRegs
staticFieldHandler wbr = do
  printfTrap "patching static field handler\n"
  -- patch the offset here, first two bytes are part of the insn (opcode + reg)
  let imm_ptr = intPtrToPtr (fromIntegral ((wbr M.! eip) + 2)) :: Ptr CPtrdiff
  checkMe <- peek imm_ptr
  if checkMe == 0x00000000 then
    do
      getStaticFieldAddr (wbr M.! eip) >>= poke imm_ptr
      return wbr
    else error "staticFieldHandler: something is wrong here. abort.\n"

patchInvoke :: MethodInfo -> CPtrdiff -> CPtrdiff -> IO NativeWord ->
               WriteBackRegs -> CodeGen e s WriteBackRegs
patchInvoke (MethodInfo methname _ msig)  method_table table2patch io_offset wbr = do
  liftIO $ printfTrap "patching invoke call\n"
  vmap <- liftIO getVirtualMap
  let calltype = if method_table == table2patch then "virtual" else "interface"
  liftIO $ printfTrap $ printf "patched %s call: issued from 0x%08x\n" (calltype :: String) (fromIntegral (wbr M.! eip) :: Word32)
  when (method_table == 0) $ error "patchInvoke: method_table is null.  abort."
  let cls = vmap M.! fromIntegral method_table
  liftIO $ printfTrap $ printf "cls stuff: %s\n" (toString cls)
  let newmi = MethodInfo methname cls msig
  offset <- liftIO io_offset
  entryAddr <- liftIO $ lookupMethodEntry newmi
  call32Eax (Disp offset)
  -- patch entry in table
  let call_insn = intPtrToPtr . fromIntegral $ table2patch + fromIntegral offset
  liftIO $ poke call_insn entryAddr
  liftIO $ printfTrap $ printf "patched virtual call: 0x%08x\n" (fromIntegral entryAddr :: Word32)
  return wbr

-- harpy helper (don't cut immediates)

-- call disp32(%eax)
call32Eax :: Disp -> CodeGen e s ()
call32Eax (Disp disp32) = emit8 0xff >> emit8 0x90 >> emit32 disp32
