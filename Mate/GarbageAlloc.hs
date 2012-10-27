{-# LANGUAGE ForeignFunctionInterface #-}
module Mate.GarbageAlloc(
    mallocClassData,
    mallocStringGC,
    mallocObjectGC,
    mallocObjectGC_stackstrace,
    getHeapMemory,
    printMemoryUsage,
    printGCStats,
    mallocObjectUnmanaged,
    mallocStringUnmanaged)  where

import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe

import qualified Data.ByteString.Internal as BI

import Mate.GC.Boehm
import Mate.StackTrace
import Mate.MemoryManager

import Mate.Debug

-- unified place for allocating Memory

mallocObjectUnmanaged :: Int -> IO CPtrdiff
mallocObjectUnmanaged size = do
  ptr <- mallocBytes size
  BI.memset (castPtr ptr) 0 (fromIntegral size)
  printfStr $ printf "mallocObjectUnmanged: %d\n" size
  return $ fromIntegral $ ptrToIntPtr ptr

mallocStringUnmanaged :: Int -> IO (Ptr a)
mallocStringUnmanaged size = do
  printfStr $ printf "mallocStringUnamaged: %d\n" size
  ptr <- mallocBytes size
  BI.memset (castPtr ptr) 0 (fromIntegral size)
  return ptr

mallocClassData :: Int -> IO (Ptr a)
mallocClassData size = do
  printfStr $ printf "mallocClassData: %d\n" size
  mem <- mallocBytes size
  BI.memset (castPtr mem) 0 (fromIntegral size)
  addRootGC mem (plusPtr mem size)
  return mem

mallocStringGC :: Int -> IO (Ptr a)
mallocStringGC size = do
  printfStr $ printf "mallocString: %d\n" size
  ptr <- mallocBytesGC size
  BI.memset (castPtr ptr) 0 (fromIntegral size)
  return ptr

foreign export ccall mallocObjectGC :: Int -> IO CPtrdiff
mallocObjectGC :: Int -> IO CPtrdiff
mallocObjectGC size = do
  ptr <- mallocBytesGC size
  BI.memset (castPtr ptr) 0 (fromIntegral size)
  printfStr $ printf "mallocObject: %d\n" size
  return $ fromIntegral $ ptrToIntPtr ptr

foreign export ccall mallocObjectGC_stackstrace :: CPtrdiff -> CPtrdiff -> Int -> IO CPtrdiff
mallocObjectGC_stackstrace :: CPtrdiff -> CPtrdiff -> Int -> IO CPtrdiff
mallocObjectGC_stackstrace sptr rebp size = do
  printfStr $ printf "mallocObject: %d\n" size
  printfStr $ printf "ebp @ malloc: 0x%08x\n" (fromIntegral rebp :: Word32)
  --printStackTrace 0 rebp TODO: compare performance of printStackTrace and printStackTrace'?
  printStackTrace' sptr rebp
  ptr <- mallocBytesGC size
  BI.memset (castPtr ptr) 0 (fromIntegral size)
  return $ fromIntegral $ ptrToIntPtr ptr


{-# NOINLINE twoSpaceGC #-}
twoSpaceGC :: TwoSpace
twoSpaceGC = unsafePerformIO $ initTwoSpace 0x1000000

getHeapMemory :: IO Int
getHeapMemory = getHeapSizeGC

foreign export ccall printMemoryUsage :: IO ()
printMemoryUsage :: IO ()
printMemoryUsage = getHeapMemory >>= print

foreign export ccall printGCStats :: IO ()
printGCStats :: IO ()
printGCStats = putStrLn "Should print GC Stats"
