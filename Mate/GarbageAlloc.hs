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

import Foreign hiding (unsafePerformIO, mallocBytes)
import qualified Foreign as F
import Foreign.C
import System.IO.Unsafe
import Data.IORef
import Control.Monad.State

import qualified Data.ByteString.Internal as BI
import qualified Data.Set as S

import Mate.GC.Boehm
import Mate.StackTrace
import Mate.MemoryManager

import Mate.Debug

-- unified place for allocating Memory

mallocObjectUnmanaged :: Int -> IO CPtrdiff
mallocObjectUnmanaged size = do
  ptr <- F.mallocBytes size
  BI.memset (castPtr ptr) 0 (fromIntegral size)
  printfStr $ printf "mallocObjectUnmanged: %d\n" size
  return $ fromIntegral $ ptrToIntPtr ptr

mallocStringUnmanaged :: Int -> IO (Ptr a)
mallocStringUnmanaged size = do
  printfStr $ printf "mallocStringUnamaged: %d\n" size
  ptr <- F.mallocBytes size
  BI.memset (castPtr ptr) 0 (fromIntegral size)
  return ptr

mallocClassData :: Int -> IO (Ptr a)
mallocClassData size = do
  printfStr $ printf "mallocClassData: %d\n" size
  mem <- F.mallocBytes size
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
  ptr <- if usePreciseGC 
          then allocObjAndDoGCPrecise sptr rebp size
          else allocObjAndDoGC  sptr rebp size
  BI.memset (castPtr ptr) 0 (fromIntegral size)
  return $ fromIntegral $ ptrToIntPtr ptr

allocObjAndDoGC :: CPtrdiff -> CPtrdiff -> Int -> IO (Ptr a) 
allocObjAndDoGC sptr rebp size = do
  --printStackTrace 0 rebp TODO: compare performance of printStackTrace and printStackTrace'?
  _ <- if mateDEBUG then printStackTrace' sptr rebp else return []
  mallocBytesGC size

getHeapMemory :: IO Int
getHeapMemory = getHeapSizeGC

foreign export ccall printMemoryUsage :: IO ()
printMemoryUsage :: IO ()
printMemoryUsage = getHeapMemory >>= print

foreign export ccall printGCStats :: IO ()
printGCStats :: IO ()
printGCStats = putStrLn "Should print GC Stats"

-- from now: very hacky and very very evil test stuff

allocObjAndDoGCPrecise :: CPtrdiff -> CPtrdiff -> Int -> IO (Ptr a)
allocObjAndDoGCPrecise sptr rebp size = do
  _ <- printStackTrace' sptr rebp
 
  memoryManager <- readIORef twoSpaceGC 
  (ptr,memoryManager') <- runStateT (performAllocationT sptr rebp size) memoryManager 
  writeIORef twoSpaceGC memoryManager'
  
  let intptr = ptrToIntPtr ptr
  modifyIORef allocatedObjsDbg (S.insert intptr)
  objs <- readIORef allocatedObjsDbg
  
  putStrLn "allocated objs: "
  printObjsDbg objs
  
  return ptr

performAllocationT :: (AllocationManager a) => CPtrdiff -> CPtrdiff -> Int -> StateT a IO (Ptr b)
performAllocationT _ _ size = do 
  --ptr <- liftIO $ mallocBytesGC size
  mallocBytesT size

printObjsDbg :: S.Set IntPtr -> IO ()
printObjsDbg = mapM_ print . S.toList

{-# NOINLINE allocatedObjsDbg #-}
allocatedObjsDbg :: IORef (S.Set IntPtr)
allocatedObjsDbg = unsafePerformIO (newIORef S.empty)

{-# NOINLINE twoSpaceGC #-}
twoSpaceGC :: IORef TwoSpace
twoSpaceGC = unsafePerformIO $ initTwoSpace 0x1000000 >>= newIORef

