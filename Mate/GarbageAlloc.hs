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

import Foreign
import Foreign.C

import qualified Data.ByteString.Internal as BI

import JVM.ClassFile

import Mate.GC.Boehm
import Mate.Types

import Mate.Debug

-- unified place for allocating Memory
-- TODO: implement GC stuff ;-)

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

foreign export ccall mallocObjectGC_stackstrace :: CPtrdiff -> Int -> IO CPtrdiff
mallocObjectGC_stackstrace :: CPtrdiff -> Int -> IO CPtrdiff
mallocObjectGC_stackstrace rebp size = do
  printfStr $ printf "mallocObject: %d\n" size
  printfStr $ printf "ebp @ malloc: 0x%08x\n" (fromIntegral rebp :: Word32)
  stblptr <- peek (intPtrToPtr . fromIntegral $ rebp) :: IO Word32
  let sptr = castPtrToStablePtr $ intPtrToPtr $ fromIntegral stblptr
  stackinfo <- deRefStablePtr sptr :: IO RuntimeStackInfo
  printfStr $ printf "stacktrace @ malloc: %s\n" (toString $ rsiMethodname stackinfo)
  ptr <- mallocBytesGC size
  BI.memset (castPtr ptr) 0 (fromIntegral size)
  return $ fromIntegral $ ptrToIntPtr ptr

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


getHeapMemory :: IO Int
getHeapMemory = getHeapSizeGC

foreign export ccall printMemoryUsage :: IO ()
printMemoryUsage :: IO ()
printMemoryUsage = getHeapMemory >>= print

foreign export ccall printGCStats :: IO ()
printGCStats :: IO ()
printGCStats = putStrLn "Should print GC Stats"
