{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
#include "debug.h"
module Mate.GarbageAlloc(
    mallocClassData,
    mallocStringGC,
    mallocObjectGC,
    getHeapMemory,
    printMemoryUsage,
    printGCStats,
    mallocObjectUnmanaged,
    mallocStringUnmanaged)  where

import Foreign
import Foreign.C

import Mate.GC.Boehm

#ifdef DBG_STR
import Text.Printf
#endif
import Mate.Debug

-- unified place for allocating Memory
-- TODO: implement GC stuff ;-)

mallocClassData :: Int -> IO (Ptr a)
mallocClassData size = do
  printfStr "mallocClassData: %d\n" size
  mem <- mallocBytes size
  addRootGC mem (plusPtr mem size)
  return mem

mallocStringGC :: Int -> IO (Ptr a)
mallocStringGC size = do
  printfStr "mallocString: %d\n" size
  mallocBytesGC size

foreign export ccall mallocObjectGC :: Int -> IO CPtrdiff
mallocObjectGC :: Int -> IO CPtrdiff
mallocObjectGC size = do
  ptr <- mallocBytesGC size
  printfStr "mallocObject: %d\n" size
  return $ fromIntegral $ ptrToIntPtr ptr

mallocObjectUnmanaged :: Int -> IO CPtrdiff
mallocObjectUnmanaged size = do
  ptr <- mallocBytes size
  printfStr "mallocObjectUnmanged: %d\n" size
  return $ fromIntegral $ ptrToIntPtr ptr

mallocStringUnmanaged :: Int -> IO (Ptr a)
mallocStringUnmanaged size = do
  printfStr "mallocStringUnamaged: %d\n" size
  mallocBytes size


getHeapMemory :: IO Int
getHeapMemory = getHeapSizeGC

foreign export ccall printMemoryUsage :: IO ()
printMemoryUsage :: IO ()
printMemoryUsage = getHeapMemory >>= print

foreign export ccall printGCStats :: IO ()
printGCStats :: IO ()
printGCStats = putStrLn "Should print GC Stats"
