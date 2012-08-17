{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#include "debug.h"
module Mate.X86TrapHandling (
  mateHandler,
  register_signal
  ) where

import Numeric
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B

import Foreign
import Foreign.C.Types

import Mate.Types
import Mate.NativeSizes
import {-# SOURCE #-} Mate.MethodPool
import Mate.ClassPool

foreign import ccall "register_signal"
  register_signal :: IO ()

foreign export ccall mateHandler :: CPtrdiff -> CPtrdiff -> CPtrdiff -> CPtrdiff -> IO CPtrdiff
mateHandler :: CPtrdiff -> CPtrdiff -> CPtrdiff -> CPtrdiff -> IO CPtrdiff
mateHandler eip eax ebx esi = do
  tmap <- getTrapMap
  case M.lookup (fromIntegral eip) tmap of
    (Just (StaticMethod _)) -> staticCallHandler eip
    (Just (StaticField _))  -> staticFieldHandler eip
    (Just (InstanceOf cn))  -> instanceOfMissHandler eip cn
    (Just (NewObject cn))   -> newObjectHandler eip cn
    (Just (VirtualCall False _ io_offset)) -> invokeHandler eax eax eip io_offset
    (Just (VirtualCall True  _ io_offset)) -> invokeHandler ebx eax eip io_offset
    Nothing -> case esi of
        0x13371234 -> return (-1)
        _ -> error $ "getTrapType: abort :-(" ++ (showHex eip "") ++ ", " ++ show (M.keys tmap)

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

newObjectHandler :: CPtrdiff -> B.ByteString -> IO CPtrdiff
newObjectHandler eip classname = do
  let push_insn_ptr = intPtrToPtr (fromIntegral eip) :: Ptr CUChar
  let push_imm_ptr = intPtrToPtr (fromIntegral (eip + 1)) :: Ptr CPtrdiff
  let mov_imm_ptr = intPtrToPtr (fromIntegral (eip + 16)) :: Ptr CPtrdiff
  checkMe <- peek mov_imm_ptr
  if checkMe == 0x13371337
    then do
      objsize <- getObjectSize classname
      mtable <- getMethodTable classname
      poke push_insn_ptr 0x68 -- push_imm insn
      poke push_imm_ptr (fromIntegral objsize)
      poke mov_imm_ptr (fromIntegral mtable)
      return eip
    else error "newObjectHandler: something is wrong here. abort.\n"

invokeHandler :: CPtrdiff -> CPtrdiff -> CPtrdiff -> IO NativeWord -> IO CPtrdiff
invokeHandler method_table table2patch eip io_offset = do
  let call0_insn_ptr = intPtrToPtr (fromIntegral eip) :: Ptr CUChar
  let call1_insn_ptr = intPtrToPtr (fromIntegral (eip + 1)) :: Ptr CUChar
  let call_imm_ptr = intPtrToPtr (fromIntegral (eip + 2)) :: Ptr CPtrdiff
  offset <- io_offset
  -- @table2patch: note, that can be a method-table or a interface-table
  entryAddr <- getMethodEntry eip method_table

  -- patch table
  let call_insn = intPtrToPtr . fromIntegral $ table2patch + fromIntegral offset
  poke call_insn entryAddr

  -- patch insn
  checkMe <- peek call_imm_ptr
  if checkMe == 0x90909090
    then do
      poke call0_insn_ptr 0xff -- indirect call op[0]
      poke call1_insn_ptr 0x90 -- indirect call op[1]
      poke call_imm_ptr (fromIntegral offset)
      return eip
    else error "invokeHandler: something is wrong here. abort\n"
