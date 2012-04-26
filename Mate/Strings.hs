{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Mate.Strings (
  getUniqueStringAddr
  ) where

import Data.Word
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Internal as BI
import Text.Printf

import Foreign.Ptr
import Foreign.ForeignPtr
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

-- TOOD(bernhard): quite hackish
allocateJavaString :: B.ByteString -> IO Word32
allocateJavaString str = do
  let strlen = fromIntegral $ B.length str
  let str_unpacked = (map fromIntegral $ B.unpack str) :: [Word8]
  arr <- newArray str_unpacked
  newstr <- BI.create strlen (\x -> BI.memcpy x arr (fromIntegral strlen))
  let (newstrptr, _, _) = BI.toForeignPtr newstr
  let w32_ptr = fromIntegral $ ptrToIntPtr $ unsafeForeignPtrToPtr newstrptr
  printf "new str ptr: 0x%08x\n" w32_ptr
  return w32_ptr
