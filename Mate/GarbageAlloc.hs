{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
#include "debug.h"
module Mate.GarbageAlloc where

import Foreign
import Foreign.C

#ifdef DEBUG
import Text.Printf
#endif
import Mate.Debug

-- unified place for allocating Memory
-- TODO: implement GC stuff ;-)

mallocClassData :: Int -> IO (Ptr a)
mallocClassData size = do
  printfStr "mallocClassData: %d\n" size
  mallocBytes size

mallocString :: Int -> IO (Ptr a)
mallocString size = do
  printfStr "mallocString: %d\n" size
  mallocBytes size

foreign export ccall mallocObject :: Int -> IO CPtrdiff
mallocObject :: Int -> IO CPtrdiff
mallocObject size = do
  ptr <- mallocBytes size
  printfStr "mallocObject: %d\n" size
  return $ fromIntegral $ ptrToIntPtr ptr
