{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
#include "debug.h"
module Mate.Strings (
  getUniqueStringAddr
  ) where

import Data.Word
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Internal as BI
#ifdef DEBUG
import Text.Printf
#endif

import JVM.ClassFile

import Foreign
import Foreign.C.Types

import Mate.Types
import Mate.ClassPool
import Mate.Debug
import Mate.GarbageAlloc


getUniqueStringAddr :: B.ByteString -> IO Word32
getUniqueStringAddr str = do
  smap <- getStringMap
  case M.lookup str smap of
    Nothing -> do
      addr <- allocateJavaString str
      setStringMap $ M.insert str addr smap
      return addr
    Just addr -> return addr

allocateJavaString :: B.ByteString -> IO Word32
allocateJavaString str = do
  {- we have to build a java object layout here, where String object looks like
   -
   -  this -+
   -        |
   -        v
   -  +-------------+-------+-------+----------------+--------+
   -  | MethodTable | value | count | cachedhashcode | offset |
   -  +-------------+-------+-------+----------------+--------+
   -        |           |
   -        |           +------------+
   -        v                        v
   -  java/lang/String           +--------+--------+--------+-----+------------------+
   -                             | length | str[0] | str[1] | ... | str [length - 1] |
   -                             +--------+--------+--------+-----+------------------+
   -  all cells are 32bit wide, except str[i] of course (they're 8bit [but
   -  should be 16bit, TODO]).
   -}
  -- build object layout
  fsize <- getObjectSize "java/lang/String"
  printfStr "string: fsize: %d (should be 4 * 5)\n" fsize
  tblptr <- mallocObject $ fromIntegral fsize
  let ptr = intPtrToPtr (fromIntegral tblptr) :: Ptr CPtrdiff
  mtbl <- getMethodTable "java/lang/String"
  poke ptr $ fromIntegral mtbl

  -- build array layout
  let strlen = fromIntegral $ B.length str
  -- (+1) for \0, (+4) for length
  newstr <- mallocString (strlen + 5)
  BI.memset newstr 0 (fromIntegral $ strlen + 5)
  arr <- newArray ((map fromIntegral $ B.unpack str) :: [Word8])
  copyBytes (plusPtr newstr 4) arr strlen
  printfStr "new str ptr: (%s)@%d\n" (toString str) strlen

  let newstr_length = castPtr newstr :: Ptr CPtrdiff
  poke newstr_length $ fromIntegral strlen

  -- set value pointer
  poke (plusPtr ptr 4) (fromIntegral (ptrToIntPtr newstr) :: CPtrdiff)
  -- set count field
  poke (plusPtr ptr 8) (fromIntegral strlen :: CPtrdiff)
  -- set hash code (TODO)
  poke (plusPtr ptr 12) (0 :: CPtrdiff)
  -- set offset
  poke (plusPtr ptr 16) (0 :: CPtrdiff)

  return $ fromIntegral tblptr
