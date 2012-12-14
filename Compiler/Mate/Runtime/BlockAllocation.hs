{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Compiler.Mate.Runtime.BlockAllocation where

import Foreign hiding ((.&.),unsafePerformIO)
import System.IO.Unsafe(unsafePerformIO)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State

import qualified Data.Sequence as Q
import Data.Sequence ((|>))
import Data.IORef
import Text.Printf
import qualified Data.Map as M
import Data.Map(Map,(!))
import Data.Set(Set)
import qualified Data.Set as S
import Compiler.Mate.Runtime.RtsOptions
import Compiler.Mate.Debug
import qualified Compiler.Mate.Runtime.GC as GC

blockSize :: Int
blockSize = 1 `shift` blockSizePowerOfTwo

data Block = Block { beginPtr :: !IntPtr
                   , endPtr   :: !IntPtr
                   , freePtr  :: !IntPtr
                   } deriving (Eq)

instance Show Block where
    show x = printf "Begin: 0x%08x, End: 0x%08x, FreePtr: 0x%08x" (fromIntegral $ beginPtr x :: Int) (fromIntegral $ endPtr x :: Int) (fromIntegral $ freePtr x :: Int)

-- Maps number of free bytes to a set of blocks with this
-- amount of free memory
type Blocks = Map Int [Block]

data GenState = GenState { freeBlocks :: [Block]
                         , activeBlocks :: Blocks
                         , collections :: !Int
                         , generation :: Int
                         } deriving (Show,Eq)

data GcState = GcState { generations :: Map Int GenState, 
                         allocs :: Int,
                         allocatedBytes :: Int,
                         loh :: Set IntPtr,
                         allocState :: AllocC
                       } deriving (Eq,Show)

generation0 :: GcState -> GenState
generation0 s = generations s !0

emptyGenState ::  GenState
emptyGenState = GenState { freeBlocks = [], activeBlocks = M.empty, collections = 0, generation = 0 }

mkGenState :: Int -> GenState
mkGenState n = GenState { freeBlocks = [], activeBlocks = M.empty, collections = 0, generation = n }

mkGcState ::  GenState -> GcState
mkGcState s = GcState { generations = M.insert 0 s M.empty, allocs = 0, allocatedBytes = 0, loh = S.empty, allocState = error "not implemented"}

type Generation = Int

class Monad m => Alloc a m | a -> m where
    alloc ::  Generation -> Int -> StateT a m Block 
    release :: Block -> StateT a m ()

type GenStateT m a = StateT GenState (StateT a m)
type GcStateT m a = StateT GcState (StateT a m) 


-- | allocates memory within a generation
allocGen :: Alloc a m => Int -> GenStateT m a (Ptr b)
allocGen size = do
    -- let's see if there is some free memory in our blocks
    -- as heuristics, take the one with the most free memory
    current <- get
    let possibleBlocks = activeBlocks current
        biggestBlockM = M.maxViewWithKey (M.filter (not . null) possibleBlocks)
    case biggestBlockM of                          
      Just ((space,block:rest),smallBlocks) -> 
        if space >= size
          then do --awesome. we got a block which is big enough
                  let (ptr,block') = allocateInBlock block size
                  let active' = M.insert space rest smallBlocks
                      active'' = M.insertWith (++) (freeSpace block') [block'] active'
                  put current { activeBlocks = active'' }
                  return ptr 
          else do
                allocateInFreshBlock (tracePipe ("current blocks:" ++ show possibleBlocks) size)
      _ -> tracePipe ("noActiveBlocks!!" ++ show possibleBlocks ++ "WIGH M:" ++ show biggestBlockM) $ allocateInFreshBlock size

freeSpace :: Block -> Int
freeSpace Block { freePtr = free', endPtr = end } = fromIntegral $ end - free'

allocateInFreshBlock :: Alloc a m => Int -> GenStateT m a (Ptr b)
allocateInFreshBlock size = do
    current <- get
    freeBlock <- case freeBlocks current of
                 [] -> lift $ alloc blockSize (generation current) -- make a block
                 (x:xs) -> do --reuse idle block
                              put current { freeBlocks = xs }
                              return x
    let (ptr,block) = allocateInBlock freeBlock size
    activateBlock block
    return ptr

activateBlock :: Monad m => Block -> GenStateT m a ()
activateBlock b = do
    current <- get
    let active = activeBlocks current
    put current { activeBlocks = M.insertWith (++) (freeSpace b) [b] active }

allocateInBlock :: Block -> Int -> (Ptr b, Block)
allocateInBlock b@(Block { freePtr = free', endPtr = end }) size = 
    if freePtr' > end
      then error $ "allocateInBlock has insufficient space. wtf" ++ (show b) ++ " with alloc size: " ++ (show size)
      else (intPtrToPtr free', b { freePtr = freePtr' })
  where freePtr' = free' + fromIntegral size


-- | allocates memory in generation 0
allocGen0 :: Alloc a m => GC.GenInfo -> Int -> GcStateT m a (Ptr b)
allocGen0 gen size = 
    if size > blockSize 
      then  error $ "tried to allocate superhuge object in gen0 (" ++ show size ++ " bytes)"
      else do
            let targetGenIndex = GC.targetGen gen
            targetGen <- liftM (\x -> generations x!targetGenIndex) get
            (ptr, newState) <- lift $ runStateT (allocGen size) targetGen
            c <- get
            put $ c { generations = M.insert targetGenIndex newState (generations c) }
            return ptr




runBlockAllocatorC :: GC.GenInfo -> Int -> StateT GcState IO (Ptr b)
runBlockAllocatorC gen size = do
    current <- get
    let m = runStateT (allocGen0 gen size) current
    ((ptr,gcState),allocState') <- liftIO $ runStateT m (allocState current)
    put gcState { allocState = allocState' }
    return ptr


data AllocC = AllocC { freeBlocksC :: Q.Seq Block } 
                deriving (Show,Eq)

instance Alloc AllocC IO where
    alloc = allocC
    release = releaseC


mkAllocC :: Int -> IO AllocC
mkAllocC 0 = return AllocC { freeBlocksC = Q.empty }
mkAllocC n = do
    printfGc $ printf "heapSize = %d * blockSize = %d => %d\n" n blockSize (n*blockSize)
    let size' = n * blockSize
    ptr <- mallocBytes size'
    let intPtr = ptrToIntPtr ptr
    printfGc $ printf "allocated cached block memory: %s\n" (show ptr)
    let begin = shift (shift intPtr (-blockSizePowerOfTwo)) blockSizePowerOfTwo
    printfGc $ printf "starting at: 0x%08x\n" (fromIntegral begin :: Int)
    printfGc $ printf "ending at: 0x%08x\n" (fromIntegral  begin + size' :: Int)
    let allBlockBegins = [begin,begin+fromIntegral blockSize..begin + fromIntegral size']
    let allBlocks = [Block { beginPtr = x+4, endPtr = x+fromIntegral size', freePtr = x+4} | x <- allBlockBegins]
    return AllocC { freeBlocksC = Q.fromList allBlocks } -- all is free
  

allocC :: Generation -> Int -> StateT AllocC IO Block
allocC gen _ = do
    current <- get
    if Q.null (freeBlocksC current) 
      then error "out of heap memory!"
      else do
        let block = Q.index (freeBlocksC current) 0 
        writeGenToBlock block gen
        liftIO $ modifyIORef activeBlocksCnt (+ (1))
        activeOnes <- liftIO  $ readIORef activeBlocksCnt
        liftIO $ printfGc $ printf "activated a block %d\n" activeOnes
        put current { freeBlocksC = Q.drop 1 (freeBlocksC current) }
        --liftIO $ printfGc $ printf "we got free blocks: %s" (show $ length xs)
        return block { freePtr = beginPtr block }

writeGenToBlock :: Block -> Generation -> StateT AllocC IO ()
writeGenToBlock block gen = 
    liftIO $ poke (intPtrToPtr $ beginPtr block - 4) gen


releaseC :: Block -> StateT AllocC IO ()
releaseC b = do
    current' <- get
    liftIO $ modifyIORef activeBlocksCnt (+ (-1))
    activeOnes <- liftIO  $ readIORef activeBlocksCnt
    liftIO $ printfGc $ printf "released a block %d\n" activeOnes
    put current' { freeBlocksC = freeBlocksC current' |> b }

activeBlocksCnt :: IORef Int
activeBlocksCnt = unsafePerformIO $ newIORef 0

freeGensIOC :: [GenState] -> StateT GcState IO ()
freeGensIOC xs = do 
    current <- get
    let blocksToDispose = concatMap ( concatMap snd . M.toList . activeBlocks ) xs
    (_,s) <- liftIO $ runStateT (mapM_ releaseC blocksToDispose) (allocState current)
    put current { allocState = s }
    return ()


