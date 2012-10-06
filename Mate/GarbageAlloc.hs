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
import Data.String.Utils
import Control.Monad
import Debug.Trace

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
  --printStackTrace 0 rebp TODO: compare performance of printStackTrace and printStackTrace'?
  printStackTrace' rebp
  ptr <- mallocBytesGC size
  BI.memset (castPtr ptr) 0 (fromIntegral size)
  return $ fromIntegral $ ptrToIntPtr ptr

type StackDescription = (CPtrdiff, RuntimeStackInfo)

cPtrToIntPtr :: CPtrdiff -> Ptr a 
cPtrToIntPtr = intPtrToPtr . fromIntegral

-- accumulator (tailrecursive) stackframes stream (may be written as predefined function?)
stackFrames :: [StackDescription] -> CPtrdiff -> IO [StackDescription]
stackFrames accum rebp = do 
    stblptr <- peek (cPtrToIntPtr rebp) :: IO Word32
    let sptr = castPtrToStablePtr $ intPtrToPtr $ fromIntegral stblptr
    stackinfo <- deRefStablePtr sptr :: IO RuntimeStackInfo
    let accum' = (rebp,stackinfo) : accum
    if bottomOfStack stackinfo 
     then return accum -- done here. bottomOfStack claims that there are no frames left
     else -- otherwise grab the next frame, put current frame into list and continue
          peek (cPtrToIntPtr (rebp + 4)) >>= stackFrames accum'

printStackTrace' :: CPtrdiff -> IO ()
printStackTrace' ptr = do 
  printfStr "Stacktrace:\n\n"
  frames <- stackFrames [] ptr -- build with cps toget rid of reverse?
  forM_ (reverse frames) (printfStr . printf "---> %s\n" . toString . rsiMethodname . snd)  
  printfStr "End of Stack\n\n"         

bottomOfStack :: RuntimeStackInfo -> Bool
bottomOfStack = mainOrInit . toString . rsiMethodname

-- | Determines wheter a method signature (as found in RuntimeStackInfos) is bottom of stack
mainOrInit :: String -> Bool
mainOrInit sig | startswith "main" sig    = True
               | startswith "<clinit>" sig = True
               | otherwise = False

-- | Prints stacktrace until bottom of stack is reached (or native code transition or trap [TODO]
-- The Int argument describes current stack depth (for pretty printing)
printStackTrace :: Int -> CPtrdiff -> IO ()
printStackTrace depth rebp = do 
    stblptr <- peek (intPtrToPtr . fromIntegral $ rebp) :: IO Word32
    let sptr = castPtrToStablePtr $ intPtrToPtr $ fromIntegral stblptr
    stackinfo <- deRefStablePtr sptr :: IO RuntimeStackInfo
    bottomOfStack <- printFrame depth mainOrInit stackinfo
    unless bottomOfStack continue
  where continue = peek (intPtrToPtr . fromIntegral $ (rebp + 4)) >>= printStackTrace (depth+1)

-- | Prints stackframe to printStr. Returns True if bottom of the stack (i.e. main)
-- is reached.
printFrame :: Int -> (String -> Bool) -> RuntimeStackInfo -> IO Bool
printFrame d bottomCheck = print . toString . rsiMethodname
  where print sig  | bottomCheck sig 
                      = (printfStr $ printf "reached bottom of stack [%d]\n" d) >> return True
                   | otherwise 
                      = (printfStr $ printf "stacktrace @ malloc: %s [%d]\n" sig d) >> return False


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
