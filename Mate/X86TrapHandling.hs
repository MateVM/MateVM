{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#include "debug.h"
module Mate.X86TrapHandling where

import qualified Data.Map as M

import Foreign
import Foreign.C.Types

import Mate.Types
import Mate.MethodPool
import Mate.ClassPool

foreign import ccall "register_signal"
  register_signal :: IO ()


getTrapType :: CUInt -> CUInt -> IO CUInt
getTrapType signal_from from2 = do
  tmap <- getTrapMap
  case M.lookup (fromIntegral signal_from) tmap of
    (Just (MI _)) -> return 0
    (Just (SFI _)) -> return 2
    (Just _) -> error "getTrapMap: doesn't happen"
    -- maybe we've a hit on the second `from' value
    Nothing -> case M.lookup (fromIntegral from2) tmap of
      (Just (VI True _)) -> return 1
      (Just (VI False _)) -> return 5
      (Just (II True _)) -> return 4
      (Just (II False _)) -> return 8
      (Just _) -> error "getTrapType: abort #1 :-("
      Nothing -> error $ "getTrapType: abort #2 :-(" ++ show signal_from ++ ", " ++ show from2 ++ ", " ++ show tmap

foreign export ccall mateHandler :: CUInt -> CUInt -> CUInt -> CUInt -> IO CUInt
mateHandler :: CUInt -> CUInt -> CUInt -> CUInt -> IO CUInt
mateHandler eip eax ebx esp = do
  callerAddr <- callerAddrFromStack esp
  blah <- getTrapType eip callerAddr
  case blah of
    0 -> staticCallHandler eip
    1 -> invokeHandler eax eax esp True
    5 -> invokeHandler eax eax esp False
    4 -> invokeHandler eax ebx esp True
    8 -> invokeHandler eax ebx esp False
    2 -> staticFieldHandler eip
    x -> error $ "wtf: " ++ show x

staticCallHandler :: CUInt -> IO CUInt
staticCallHandler eip = do
  -- the actual insn to patch is displaced by two bytes
  let insn_ptr = intPtrToPtr (fromIntegral (eip - 2)) :: Ptr CUChar
  -- call offset is displaced by one byte
  let imm_ptr = intPtrToPtr (fromIntegral (eip - 1)) :: Ptr CUInt
  -- in codegen we set the immediate to some magic value
  -- in order to produce a SIGILL signal. we also do a safety
  -- check here, if we're really the "owner" of this signal.
  checkMe <- peek imm_ptr
  case checkMe == 0x90ffff90 of
    True -> do
      entryAddr <- getMethodEntry eip 0
      poke insn_ptr 0xe8 -- call opcode
      -- it's a relative call, so we have to calculate the offset. why "+ 3"?
      -- (1) the whole insn is 5 bytes long
      -- (2) begin of insn is displaced by 2 bytes
      -- (3) offset is calculated wrt to the beginning of the next insn
      poke imm_ptr (entryAddr - (eip + 3))
      return (eip - 2)
    False -> error "staticCallHandler: something is wrong here. abort\n"

staticFieldHandler :: CUInt -> IO CUInt
staticFieldHandler eip = do
  -- patch the offset here, first two bytes are part of the insn (opcode + reg)
  let imm_ptr = intPtrToPtr (fromIntegral (eip + 2)) :: Ptr CUInt
  checkMe <- peek imm_ptr
  case checkMe == 0x00000000 of
    True -> do
      getStaticFieldAddr eip >>= poke imm_ptr
      return eip
    False -> error "staticFieldHandler: something is wrong here. abort.\n"

invokeHandler :: CUInt -> CUInt -> CUInt -> Bool -> IO CUInt
invokeHandler method_table table2patch esp imm8 = do
  -- table2patch: note, that can be a method-table or a interface-table
  callerAddr <- callerAddrFromStack esp
  offset <- if imm8 then offsetOfCallInsn8 esp else offsetOfCallInsn32 esp
  entryAddr <- getMethodEntry callerAddr method_table
  let call_insn = intPtrToPtr (fromIntegral $ table2patch + fromIntegral offset)
  poke call_insn entryAddr
  return entryAddr


callerAddrFromStack :: CUInt -> IO CUInt
callerAddrFromStack = peek . intPtrToPtr . fromIntegral

offsetOfCallInsn8 :: CUInt -> IO CUInt
offsetOfCallInsn8 esp = do
  let ret_ptr = intPtrToPtr (fromIntegral esp) :: Ptr CUInt
  ret <- peek ret_ptr
  retval <- peek (intPtrToPtr (fromIntegral (ret - 1)) :: Ptr CUChar)
  return $ fromIntegral retval

offsetOfCallInsn32 :: CUInt -> IO CUInt
offsetOfCallInsn32 esp = do
  let ret_ptr = intPtrToPtr (fromIntegral esp) :: Ptr CUInt
  ret <- peek ret_ptr
  peek (intPtrToPtr $ fromIntegral (ret - 4))
