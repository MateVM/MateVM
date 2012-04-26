{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Mate.Strings (
  getUniqueStringAddr
  ) where

import Data.Word
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Text.Printf

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Marshal.Array

import Mate.Types


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
  -- (+1) for \0
  let strlen = (+1) $ fromIntegral $ B.length str
  arr <- newArray $ ((map fromIntegral $ B.unpack str) :: [Word8])
  newstr <- mallocBytes strlen
  copyBytes newstr arr strlen
  let w32_ptr = fromIntegral $ ptrToIntPtr newstr
  printf "new str ptr: 0x%08x (%s)@%d\n" w32_ptr (toString str) strlen
  return w32_ptr
