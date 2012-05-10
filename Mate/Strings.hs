{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
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

import Foreign.Ptr
import Foreign.Marshal.Utils
import Foreign.Marshal.Array

import Mate.Types
import Mate.Debug
import Mate.GarbageAlloc


getUniqueStringAddr :: B.ByteString -> IO Word32
getUniqueStringAddr str = do
  smap <- get_stringsmap >>= ptr2stringsmap
  case M.lookup str smap of
    Nothing -> do
      addr <- allocateJavaString str
      let smap' = M.insert str addr smap
      stringsmap2ptr smap' >>= set_stringsmap
      return addr
    Just addr -> return addr

allocateJavaString :: B.ByteString -> IO Word32
allocateJavaString str = do
  -- TODO(bernhard): is this also true for UTF8 stuff?
  let strlen = fromIntegral $ B.length str
  arr <- newArray $ ((map fromIntegral $ B.unpack str) :: [Word8])
  -- (+1) for \0
  newstr <- mallocString (strlen + 1)
  BI.memset newstr 0 (fromIntegral $ strlen + 1)
  copyBytes newstr arr strlen
  let w32_ptr = fromIntegral $ ptrToIntPtr newstr
  printf_str "new str ptr: 0x%08x (%s)@%d\n" w32_ptr (toString str) strlen
  return w32_ptr
