{-# LANGUAGE ForeignFunctionInterface #-}
module Compiler.Mate.Runtime.GarbageAlloc(
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
import Data.String.Utils

import Mate.GC.Boehm
import Compiler.Mate.Runtime.StackTrace
import Compiler.Mate.Runtime.MemoryManager

import JVM.ClassFile
import Compiler.Mate.Debug
import Compiler.Mate.Flags
import Compiler.Mate.Types

foreign export ccall mallocObjectGC_stackstrace :: CPtrdiff -> CPtrdiff -> Int -> IO CPtrdiff
foreign export ccall mallocObjectGC :: Int -> IO CPtrdiff

-- unified place for allocating Memory

mallocObjectUnmanaged :: Int -> IO CPtrdiff
mallocObjectUnmanaged size = do
  ptr <- F.mallocBytes size
  BI.memset (castPtr ptr) 0 (fromIntegral size)
  printfMem $ printf "mallocObjectUnmanged: %d\n" size
  return $ fromIntegral $ ptrToIntPtr ptr

mallocStringUnmanaged :: Int -> IO (Ptr a)
mallocStringUnmanaged size = do
  printfMem $ printf "mallocStringUnamaged: %d\n" size
  ptr <- F.mallocBytes size
  BI.memset (castPtr ptr) 0 (fromIntegral size)
  return ptr

mallocClassData :: Int -> IO (Ptr a)
mallocClassData size = do
  printfMem $ printf "mallocClassData: %d\n" size
  mem <- F.mallocBytes size
  BI.memset (castPtr mem) 0 (fromIntegral size)
  addRootGC mem (plusPtr mem size)
  return mem

mallocStringGC :: Int -> IO (Ptr a)
mallocStringGC size = do
  let size' = size + (size `rem` 4)
  printfMem $ printf "mallocString: %d\n" size'
  ptr <- mallocBytesGC size'
  BI.memset (castPtr ptr) 0 (fromIntegral size')
  return ptr


-- | allocates gc tracked obj. for precise gc no gc will take place
-- use mallocObjectGC_stacktrace instead if gc should take place
mallocObjectGC :: Int -> IO CPtrdiff
mallocObjectGC size = do
  ptr <- alloc Nothing size
  BI.memset (castPtr ptr) 0 (fromIntegral size)
  printfMem $ printf "mallocObject: %d\n" size
  return $ fromIntegral $ ptrToIntPtr ptr

-- allocates using precise or boehmgc. the first argument describes whether
-- gc may take place (if nothing, no rebp provided and no precise gc can ever work)
alloc :: Maybe (CPtrdiff, CPtrdiff) -> Int -> IO (Ptr a)
alloc regs size = 
  if usePreciseGC 
    then allocObjAndDoGCPrecise regs size
    else allocObjBoehm regs size


-- | allocates gc tracked obj. sptr and rebp must be given. If now available use
-- mallocObjectGC instead 
mallocObjectGC_stackstrace :: CPtrdiff -> CPtrdiff -> Int -> IO CPtrdiff
mallocObjectGC_stackstrace sptr rebp size = do
  printfMem $ printf "mallocObject: %d\n" size
  printfMem $ printf "ebp @ malloc: 0x%08x\n" (fromIntegral rebp :: Word32)
  ptr <- alloc (Just (sptr, rebp)) size
  BI.memset (castPtr ptr) 0 (fromIntegral size)
  return $ fromIntegral $ ptrToIntPtr ptr

allocObjBoehm :: Maybe (CPtrdiff,CPtrdiff) -> Int -> IO (Ptr a) 
allocObjBoehm regs size = do
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

-- returns stacktrace if (sptr,rebp) is valid and the stack is
-- initiated from <clinit> calls.
getStackIfPossible :: Maybe (CPtrdiff,CPtrdiff) -> IO [StackDescription]
getStackIfPossible (Just (sptr,rebp)) = getStack sptr rebp
getStackIfPossible Nothing = printfGc "no sptr, rebp provided. no gc should take place" >> return []

-- | gets stack according to valid sptr and rebp. if stack is invalid 
-- an empty stack is returned
getStack :: CPtrdiff -> CPtrdiff -> IO [StackDescription]
getStack sptr rebp = do 
  stacktrace <- printStackTrace' sptr rebp
  if isValidTrace stacktrace
   then printfGc "valid stack\n" >> return stacktrace
   else printfGc "invalid stack for gc\n" >> return []

-- | checks whether a stack should be used to retrieve gc root set
-- i.e. GC can take place at this point
isValidTrace :: [StackDescription] -> Bool
isValidTrace (x:_) = not $ startswith "<clinit>" (toString (rsiMethodname $ stackinfo x))
isValidTrace [] = True

-- | allocates obj and performs gc if possible 
allocObjAndDoGCPrecise :: Maybe (CPtrdiff,CPtrdiff) -> Int -> IO (Ptr a)
allocObjAndDoGCPrecise regs size = do
  printfGc "allocObjAndDoGCPrecise begin"
  stack <- getStackIfPossible regs 

  let gcAction = buildGCAction stack size
 
  memoryManager <- readIORef twoSpaceGC 
  (ptr,memoryManager') <- runStateT gcAction memoryManager 
  writeIORef twoSpaceGC memoryManager'
  
  printfGc "allocObjAndDoGCPrecise end"
  return ptr

{-# NOINLINE twoSpaceGC #-}
twoSpaceGC :: IORef TwoSpace
twoSpaceGC = unsafePerformIO $ initTwoSpace 0x1000000 >>= newIORef

