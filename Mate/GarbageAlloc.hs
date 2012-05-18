{-# LANGUAGE ForeignFunctionInterface #-}
module Mate.GarbageAlloc where

import Foreign
import Foreign.C

-- unified place for allocating Memory
-- TODO: implement GC stuff ;-)

mallocClassData :: Int -> IO (Ptr a)
mallocClassData = mallocBytes

mallocString :: Int -> IO (Ptr a)
mallocString = mallocBytes

foreign export ccall mallocObject :: Int -> IO CUInt
mallocObject :: Int -> IO CUInt
mallocObject size = do
  ptr <- mallocBytes size
  return $ fromIntegral $ ptrToIntPtr ptr
