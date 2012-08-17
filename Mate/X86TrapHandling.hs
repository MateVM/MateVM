{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#include "debug.h"
module Mate.X86TrapHandling (
  mateHandler,
  register_signal
  ) where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B

import Foreign
import Foreign.C.Types

import Mate.Types
import {-# SOURCE #-} Mate.MethodPool
import Mate.ClassPool

foreign import ccall "register_signal"
  register_signal :: IO ()

data TrapType =
    StaticMethodCall
  | StaticFieldAccess
  | VirtualMethodCall Bool
  | InterfaceMethodCall Bool
  | InstanceOfMiss B.ByteString
  | NoKnownTrap String

getTrapType :: TrapMap -> CPtrdiff -> CPtrdiff -> TrapType
getTrapType tmap signal_from from2 =
  case M.lookup (fromIntegral signal_from) tmap of
    (Just (StaticMethod _)) -> StaticMethodCall
    (Just (StaticField _)) -> StaticFieldAccess
    (Just (InstanceOf cn)) -> InstanceOfMiss cn
    (Just _) -> NoKnownTrap "getTrapMap: doesn't happen"
    -- maybe we've a hit on the second `from' value
    Nothing -> case M.lookup (fromIntegral from2) tmap of
      (Just (VirtualMethod imm8 _)) -> VirtualMethodCall imm8
      (Just (InterfaceMethod imm8 _)) -> InterfaceMethodCall imm8
      (Just _) -> NoKnownTrap "getTrapType: abort #1 :-("
      Nothing -> NoKnownTrap $ "getTrapType: abort #2 :-(" ++ show signal_from ++ ", " ++ show from2 ++ ", " ++ show tmap

foreign export ccall mateHandler :: CPtrdiff -> CPtrdiff -> CPtrdiff -> CPtrdiff -> CPtrdiff -> IO CPtrdiff
mateHandler :: CPtrdiff -> CPtrdiff -> CPtrdiff -> CPtrdiff -> CPtrdiff -> IO CPtrdiff
mateHandler eip eax ebx esp esi = do
  callerAddr <- callerAddrFromStack esp
  tmap <- getTrapMap
  case getTrapType tmap eip callerAddr of
    StaticMethodCall  -> staticCallHandler eip
    StaticFieldAccess -> staticFieldHandler eip
    (InstanceOfMiss cn) -> instanceOfMissHandler eip cn
    VirtualMethodCall imm8   -> invokeHandler eax eax esp imm8
    InterfaceMethodCall imm8 -> invokeHandler eax ebx esp imm8
    NoKnownTrap err ->
      case esi of
        0x13371234 -> return (-1)
        _ -> error err

staticCallHandler :: CPtrdiff -> IO CPtrdiff
staticCallHandler eip = do
  -- the actual insn to patch as pointer
  let insn_ptr = intPtrToPtr (fromIntegral eip) :: Ptr CUChar
  -- call offset is displaced by one byte (as the first byte is the opcode)
  let imm_ptr = intPtrToPtr (fromIntegral (eip + 1)) :: Ptr CPtrdiff
  -- in codegen we set the immediate to some magic value
  -- in order to produce a SIGILL signal. we also do a safety
  -- check here, if we're really the "owner" of this signal.
  checkMe <- peek imm_ptr
  if checkMe == 0x909090ff then
    do
      entryAddr <- getMethodEntry eip 0
      poke insn_ptr 0xe8 -- `call' opcode
      -- it's a relative call, so we have to calculate the offset. why "+ 5"?
      -- (1) the whole insn is 5 bytes long
      -- (2) offset is calculated wrt to the beginning of the next insn
      poke imm_ptr (entryAddr - (eip + 5))
      return eip
    else error "staticCallHandler: something is wrong here. abort\n"

staticFieldHandler :: CPtrdiff -> IO CPtrdiff
staticFieldHandler eip = do
  -- patch the offset here, first two bytes are part of the insn (opcode + reg)
  let imm_ptr = intPtrToPtr (fromIntegral (eip + 2)) :: Ptr CPtrdiff
  checkMe <- peek imm_ptr
  if checkMe == 0x00000000 then
    do
      getStaticFieldAddr eip >>= poke imm_ptr
      return eip
    else error "staticFieldHandler: something is wrong here. abort.\n"

instanceOfMissHandler :: CPtrdiff -> B.ByteString -> IO CPtrdiff
instanceOfMissHandler eip classname = do
  -- first byte is going to be the opcode
  let insn_ptr = intPtrToPtr (fromIntegral eip) :: Ptr CUChar
  -- the next four bytes are the immediate
  let imm_ptr = intPtrToPtr (fromIntegral (eip + 1)) :: Ptr CPtrdiff
  checkMe <- peek imm_ptr
  if checkMe == 0x909090ff then -- safety check...
    do
      mtable <- getMethodTable classname
      poke imm_ptr (fromIntegral mtable)
      poke insn_ptr 0xba -- `mov edx' opcode
      return eip
    else error "instanceOfMissHandler: something is wrong here. abort.\n"

invokeHandler :: CPtrdiff -> CPtrdiff -> CPtrdiff -> Bool -> IO CPtrdiff
invokeHandler method_table table2patch esp imm8 = do
  -- table2patch: note, that can be a method-table or a interface-table
  callerAddr <- callerAddrFromStack esp
  offset <- if imm8 then offsetOfCallInsn8 esp else offsetOfCallInsn32 esp
  entryAddr <- getMethodEntry callerAddr method_table
  let call_insn = intPtrToPtr (fromIntegral $ table2patch + fromIntegral offset)
  poke call_insn entryAddr
  return entryAddr


callerAddrFromStack :: CPtrdiff -> IO CPtrdiff
callerAddrFromStack = peek . intPtrToPtr . fromIntegral

offsetOfCallInsn8 :: CPtrdiff -> IO CPtrdiff
offsetOfCallInsn8 esp = do
  let ret_ptr = intPtrToPtr (fromIntegral esp) :: Ptr CPtrdiff
  ret <- peek ret_ptr
  retval <- peek (intPtrToPtr (fromIntegral (ret - 1)) :: Ptr CUChar)
  return $ fromIntegral retval

offsetOfCallInsn32 :: CPtrdiff -> IO CPtrdiff
offsetOfCallInsn32 esp = do
  let ret_ptr = intPtrToPtr (fromIntegral esp) :: Ptr CPtrdiff
  ret <- peek ret_ptr
  peek (intPtrToPtr $ fromIntegral (ret - 4))
