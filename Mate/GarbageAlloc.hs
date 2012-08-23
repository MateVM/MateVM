{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
#include "debug.h"
module Mate.GarbageAlloc(
    mallocClassData,
    mallocString,
    mallocObject,
    getHeapMemory,
    printMemoryUsage,
    mallocStringVM,
    mallocObjectVM)  where

import Foreign
import Foreign.C

import Mate.GC.Boehm

import Text.Printf
import Mate.Debug

-- unified place for allocating Memory
-- TODO: implement GC stuff ;-)

mallocClassData :: Int -> IO (Ptr a)
mallocClassData size = do
  printfStr "mallocClassData: %d\n" size
  mem <- mallocBytes size
  addRootGC mem (plusPtr mem size)
  return mem

mallocString :: Int -> IO (Ptr a)
mallocString size = do
  printfStr "mallocString: %d\n" size
  mallocBytesGC size

mallocStringVM :: Int -> IO (Ptr a)
mallocStringVM = mallocBytes

foreign export ccall mallocObject :: Int -> IO CPtrdiff
mallocObject :: Int -> IO CPtrdiff
mallocObject size = do
  ptr <- mallocBytesGC size
  printfStr "mallocObject: %d\n" size
  return $ fromIntegral $ ptrToIntPtr ptr

mallocObjectVM :: Int -> IO CPtrdiff
mallocObjectVM size = do
  ptr <- mallocBytes size
  printfStr "mallocObject VM: %d\n" size
  return $ fromIntegral $ ptrToIntPtr ptr

-- TODO: delete me
foreign export ccall demoInterfaceCall :: CUInt -> IO ()
demoInterfaceCall :: CUInt -> IO ()
demoInterfaceCall val = do
  printf "demoInterfaceCall: 0x%08x\n" (fromIntegral val :: Word32)
  return ()

getHeapMemory :: IO Int
getHeapMemory = getHeapSizeGC


foreign export ccall printMemoryUsage :: IO ()
printMemoryUsage :: IO ()
printMemoryUsage = getHeapMemory >>= print
