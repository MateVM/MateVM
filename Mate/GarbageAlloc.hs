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
import Data.String.Utils

import Mate.GC.Boehm
import Mate.StackTrace
import Mate.MemoryManager
--import qualified Mate.JavaObjectsGC as Obj

import JVM.ClassFile
import Mate.Debug
import Mate.Types

foreign export ccall mallocObjectGC_stackstrace :: CPtrdiff -> CPtrdiff -> Int -> IO CPtrdiff
foreign export ccall mallocObjectGC :: Int -> IO CPtrdiff

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


-- | allocates gc tracked obj. for precise gc no gc will take place
-- use mallocObjectGC_stacktrace instead if gc should take place
mallocObjectGC :: Int -> IO CPtrdiff
mallocObjectGC size = do
  ptr <- alloc Nothing size
  BI.memset (castPtr ptr) 0 (fromIntegral size)
  printfStr $ printf "mallocObject: %d\n" size
  return $ fromIntegral $ ptrToIntPtr ptr

-- allocates using precise or boehmgc. the first argument describes whether
-- gc may take place (if nothing, no rebp provided and no precise gc can ever work)
alloc :: Maybe (CPtrdiff, CPtrdiff) -> Int -> IO (Ptr a)
alloc regs size = 
  if usePreciseGC 
    then allocObjAndDoGCPrecise regs size
    else allocObjAndDoGC regs size


-- | allocates gc tracked obj. sptr and rebp must be given. If now available use
-- mallocObjectGC instead 
mallocObjectGC_stackstrace :: CPtrdiff -> CPtrdiff -> Int -> IO CPtrdiff
mallocObjectGC_stackstrace sptr rebp size = do
  printfStr $ printf "mallocObject: %d\n" size
  printfStr $ printf "ebp @ malloc: 0x%08x\n" (fromIntegral rebp :: Word32)
  ptr <- alloc (Just (sptr, rebp)) size
  BI.memset (castPtr ptr) 0 (fromIntegral size)
  return $ fromIntegral $ ptrToIntPtr ptr

allocObjAndDoGC :: Maybe (CPtrdiff,CPtrdiff) -> Int -> IO (Ptr a) 
allocObjAndDoGC regs size = do
  --printStackTrace 0 rebp TODO: compare performance of printStackTrace and printStackTrace'?
  _ <- case (mateDEBUG, regs) of
        (True, Just(sptr,rebp)) -> printStackTrace' sptr rebp
        _ -> return []
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

allocObjAndDoGCPrecise :: Maybe (CPtrdiff,CPtrdiff) -> Int -> IO (Ptr a)
allocObjAndDoGCPrecise regs size = do
  stack <- case regs of 
        Just(sptr,rebp) -> do stack' <- printStackTrace' sptr rebp 
                              case stack' of
                               s@(x:_) -> if startswith "<clinit>" (toString (rsiMethodname $ stackinfo x)) 
                                            then return []
                                            else return s
                               _ -> return stack'
        _ -> return []

  printfStr "look here"
  print stack
  let gcAction = buildGCAction stack size
 
  memoryManager <- readIORef twoSpaceGC 
  (ptr,memoryManager') <- runStateT gcAction memoryManager 
  writeIORef twoSpaceGC memoryManager'
  
  let intptr = ptrToIntPtr ptr
  modifyIORef allocatedObjsDbg (S.insert intptr)
  _ <- readIORef allocatedObjsDbg
  
  --putStrLn "allocated objs: "
  --printObjsDbg objs
  
  let shifted = ptr `plusPtr` 12
  --Obj.printRef shifted
  return shifted


--printObjsDbg :: S.Set IntPtr -> IO ()
--printObjsDbg = mapM_ print . S.toList

{-# NOINLINE allocatedObjsDbg #-}
allocatedObjsDbg :: IORef (S.Set IntPtr)
allocatedObjsDbg = unsafePerformIO (newIORef S.empty)

{-# NOINLINE twoSpaceGC #-}
twoSpaceGC :: IORef TwoSpace
twoSpaceGC = unsafePerformIO $ initTwoSpace 0x1000000 >>= newIORef

