{-# LANGUAGE ForeignFunctionInterface #-}
module Compiler.Mate.Runtime.GarbageAlloc(
    mallocClassData,
    mallocStaticData,
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
import Compiler.Mate.Runtime.MemoryManager hiding (heapSize)
import Compiler.Mate.Runtime.TwoSpaceAllocator
import Compiler.Mate.Runtime.JavaObjects
import Compiler.Mate.Runtime.ClassPool
import qualified Compiler.Mate.Runtime.GenerationalGC as Gen
import qualified Compiler.Mate.Runtime.GC as GC
import qualified Compiler.Mate.Runtime.BlockAllocation as B
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

mallocStaticData :: Int -> FieldTypeMap -> IO (Ptr a)
mallocStaticData size types = do
  printfMem $ printf "mallocStaticData: %d\n" size
  mem <- F.mallocBytes size
  BI.memset (castPtr mem) 0 (fromIntegral size)
  addRootGC mem (plusPtr mem size)
  let memInt = fromIntegral $ ptrToIntPtr mem
  printfMem $ printf "got adress %s" (show mem)
  printfMem $ printf "Here we go: %s\n" (show types)
  let staticFields = [memInt + (off-1)*4 | off <- extractRerefenceTypeOffsets types]
  printfMem $ printf "classData fields: %s\n" (show $ showRefs2 staticFields)
  mapM_ (addRootPrecise . fromIntegral) staticFields
  return mem

extractRerefenceTypeOffsets :: FieldTypeMap -> [Int32]
extractRerefenceTypeOffsets = map fst . filter (isReferenceType . snd) . getFieldSignatures

showRefs2 :: Integral a => [a] -> [String]
showRefs2 = map (show . intPtrToPtr . fromIntegral) 

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
  addr <- mallocObjectGC size'
  addRootPrecise (fromIntegral addr)
  let ptr' = intPtrToPtr (fromIntegral addr)
  printfMem $ printf "string got: %s\n" (show ptr')
  BI.memset ptr' 0 (fromIntegral size')
  return ptr' 


-- | allocates gc tracked obj. for precise gc no gc will take place
-- use mallocObjectGC_stacktrace instead if gc should take place
mallocObjectGC :: Int -> IO CPtrdiff
mallocObjectGC size = do
  ptr <- alloc Nothing size
  BI.memset (castPtr ptr) 0 (fromIntegral size)
  printfMem $ printf "mallocObject: %d\n" size
  let addr = fromIntegral $ ptrToIntPtr ptr
  when (addr == 0) $ error "mallocObjectGC: ptr is null"
  return addr

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
mallocObjectGC_stackstrace eip rebp size = do
  printfMem $ printf "mallocObject: %d\n" size
  printfMem $ printf "ebp @ malloc: 0x%08x\n" (fromIntegral rebp :: Word32)
  printfMem $ printf "eip @ malloc: 0x%08x\n" (fromIntegral eip :: Word32)
  ptr <- alloc (Just (eip, rebp)) size
  BI.memset (castPtr ptr) 0 (fromIntegral size)
  return $ fromIntegral $ ptrToIntPtr ptr

allocObjBoehm :: Maybe (CPtrdiff,CPtrdiff) -> Int -> IO (Ptr a) 
allocObjBoehm regs size = do
  --printStackTrace 0 rebp TODO: compare performance of printStackTrace and printStackTrace'?
  _ <- case (mateDEBUG, regs) of
        (True, Just(sptr,rebp)) -> printStackTrace' sptr rebp
        _ -> return []
  printfMem "do alloc boehm. \n"
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

-- returns stacktrace if (eip,rebp) is valid and the stack is
-- initiated from <clinit> calls.
getStackIfPossible :: Maybe (CPtrdiff,CPtrdiff) -> IO [StackDescription]
getStackIfPossible (Just (eip,rebp)) = getStack eip rebp
getStackIfPossible Nothing = printfGc "no sptr, rebp provided. no gc should take place\n" >> return []

-- | gets stack according to valid sptr and rebp. if stack is invalid 
-- an empty stack is returned
getStack :: CPtrdiff -> CPtrdiff -> IO [StackDescription]
getStack eip rebp = do 
  stacktrace <- printStackTrace' eip rebp
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
  printfGc "allocObjAndDoGCPrecise begin...\n"
  stack <- getStackIfPossible regs 

  if useBlockAllocator 
    then do
      permRoots <- return []--readIORef permGenRoots
      gcState <- readIORef genGC

      xs <- readIORef permGenRoots
      printfGc $ printf "fuckin roots: %s" (show $ map intPtrToPtr xs)
      ys <- (mapM (peek . intPtrToPtr) xs) :: IO [IntPtr]
      printfGc $ printf "*fuckin roots: %s" (show $ map intPtrToPtr ys)

      patches <- Gen.buildPatchAction stack permRoots 

      let collectAndAlloc = (if null stack 
                              then return ()
                              else Gen.collectGen patches) >> 
                                  Gen.mallocBytesGen GC.mkGen0 size :: StateT B.GcState IO (Ptr b)

      printfGc "running statetGC\n\n"
      (ptr,gcState') <- runStateT collectAndAlloc gcState
      writeIORef genGC gcState'
      printfGc "GC finished\n\n"

      return ptr
    else do
      permRoots <- readIORef permGenRoots 
      let gcAction = buildGCAction (error "no geninfo") stack permRoots size
     
      memoryManager <- readIORef twoSpaceGC 
      (ptr,memoryManager') <- runStateT gcAction memoryManager 
      writeIORef twoSpaceGC memoryManager'
      
      printfGc "allocObjAndDoGCPrecise completed.\n"
      return ptr

{-# NOINLINE twoSpaceGC #-}
twoSpaceGC :: IORef TwoSpace
twoSpaceGC = if not useBlockAllocator
               then unsafePerformIO $ initTwoSpace 0x1000000 >>= newIORef
               else error "tried to initialize twospace allocator but block allocator is set according to Flags"

{-# NOINLINE permGenRoots #-}
permGenRoots :: IORef [IntPtr]
permGenRoots = unsafePerformIO $ newIORef []

addRootPrecise :: IntPtr -> IO ()
addRootPrecise ptr = modifyIORef permGenRoots (ptr:) 

{-# NOINLINE genGC #-}
genGC :: IORef B.GcState
genGC = if useBlockAllocator 
          then unsafePerformIO $ Gen.initGen heapSize  >>= newIORef
          else error "tried to initialize generational gc but block allocation is disabled (enable flag!)"
