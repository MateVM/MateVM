{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Compiler.Mate.Runtime.BlockAllocation where

import Foreign
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Identity
import Test.QuickCheck

import Text.Printf
import qualified Data.Map as M
import Data.Map(Map)
import Data.Set(Set)
import qualified Data.Set as S
import Compiler.Mate.Flags

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
                         } deriving (Show,Eq)

data GcState = GcState { generations :: [GenState], 
                         allocs :: Int,
                         allocatedBytes :: Int,
                         loh :: Set IntPtr
                       } deriving (Eq,Show)

generation0 :: GcState -> GenState
generation0 = head . generations

class Monad m => Alloc a m | a -> m where
    alloc :: Int -> StateT a m Block 
    release :: Block -> StateT a m ()

type GenStateT m a = StateT GenState (StateT a m)
type GcStateT m a = StateT GcState (StateT a m) 

-- This is the mock allocator
data AllocM = AllocM { freeS :: IntPtr } deriving (Eq)
instance Show AllocM where
    show x = printf "freeS: 0x%08x" (fromIntegral $ freeS x :: Int)

type AllocMT a = StateT AllocM Identity a

-- | allocates memory within a generation
allocGen :: Alloc a m => Int -> GenStateT m a (Ptr b)
allocGen size = do
    -- let's see if there is some free memory in our blocks
    -- as heuristics, take the one with the most free memory
    current <- get
    let possibleBlocks = activeBlocks current
        biggestBlockM = M.maxViewWithKey possibleBlocks
    case biggestBlockM of                          
      Just ((space,block:rest),smallBlocks) -> 
        if space >= size
          then do --awesome. we got a block which is big enough
                  let (ptr,block') = allocateInBlock block size
                  let active' = M.insert space rest smallBlocks
                      active'' = M.insertWith (++) (freeSpace block') [block'] active'
                  put current { activeBlocks = active'' }
                  return ptr 
          else allocateInFreshBlock size
      _ -> allocateInFreshBlock size

freeSpace :: Block -> Int
freeSpace Block { freePtr = free', endPtr = end } = fromIntegral $ end - free'

allocateInFreshBlock :: Alloc a m => Int -> GenStateT m a (Ptr b)
allocateInFreshBlock size = do
    current <- get
    freeBlock <- case freeBlocks current of
                 [] -> lift $ alloc blockSize -- make a block
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
      then error "allocateInBlock has insufficient space. wtf"
      else (intPtrToPtr free', b { freePtr = freePtr' })
  where freePtr' = free' + fromIntegral size


-- | allocates memory in generation 0
allocGen0 :: Alloc a m => Int -> GcStateT m a (Ptr b)
allocGen0 size = 
    if size > blockSize 
      then  error $ "tried to allocate superhuge object in gen0 (" ++ show size ++ " bytes)"
      else do
            (gen0:xs) <- liftM generations get
            (ptr, newState) <- lift $ runStateT (allocGen size) gen0
            c <- get
            put $ c { generations = newState:xs }
            return ptr

emptyAllocM :: AllocM
emptyAllocM = AllocM { freeS = 0 }

instance Alloc AllocM Identity where
    alloc = mkBlockM
    release _ = return ()

data AllocIO = AllocIO deriving Show
type AllocIOT a = StateT AllocIO IO a

instance Alloc AllocIO IO where
    alloc = mkBlockIO
    release = releaseBlockIO 

currentFreePtrM ::  AllocMT IntPtr
currentFreePtrM = liftM freeS get 

mkBlockM :: Int -> AllocMT Block
mkBlockM size = do 
  start <- currentFreePtrM
  let end = start + fromIntegral size
  put AllocM { freeS = end + 1 } -- in reality do padding here
  return Block { beginPtr = start, endPtr = end, freePtr = start }

mkBlockIO :: Int -> AllocIOT Block
mkBlockIO size = do
  ptr <- liftIO $ mallocBytes size
  return Block { beginPtr = ptrToIntPtr ptr,
                 endPtr = ptrToIntPtr $ ptr `plusPtr` size,
                 freePtr = ptrToIntPtr ptr }  
                        
releaseBlockIO :: Block -> AllocIOT ()
releaseBlockIO = liftIO . freeBlock
  where action = return . intPtrToPtr . beginPtr
        freeBlock = (free =<<) . action


blockAdresses :: Num a => a -> [(a,a)]
blockAdresses k = iterate next first
    where first = (0,k-1)
          next (l,u) = (l+k,u+k)

emptyGenState ::  GenState
emptyGenState = GenState { freeBlocks = [], activeBlocks = M.empty, collections = 0 }
gcState1 ::  GcState
gcState1 = GcState { generations = [emptyGenState], allocs = 0, allocatedBytes = 0, loh = S.empty }

mkGcState ::  GenState -> GcState
mkGcState s = GcState { generations = [s], allocs = 0, allocatedBytes = 0, loh = S.empty }


runBlockAllocator :: Int -> GcState -> IO (Ptr b, GcState)
runBlockAllocator size current = evalStateT allocT AllocIO
    where allocT = runStateT (allocGen0 size) current



--dont be too frightened here. cornholio
runTest :: StateT GcState (StateT AllocM Identity) (Ptr a) -> GcState -> AllocM -> ((Ptr a, GcState), AllocM)
runTest x gcState allocState = let allocation = runStateT x gcState
                                   resultT = runStateT allocation allocState
                                   result = runIdentity resultT
                               in result


test1 ::  ((Ptr b, GcState), AllocM)
test1 = let x = runStateT (allocGen0 12) gcState1
            y = runStateT x emptyAllocM
        in runIdentity y

test2 ::  IO ((Ptr b, GcState), AllocIO)
test2 = let x = runStateT (allocGen0 12) gcState1
            y = runStateT x AllocIO
        in y

int2Ptr :: Int -> Ptr b
int2Ptr = intPtrToPtr . fromIntegral

emptyTest :: Int -> Property
emptyTest x = let ((ptr,_),_) = runTest (allocGen0 x) start emptyAllocM 
              in x <= blockSize ==> ptr == int2Ptr 0
    where start = mkGcState  
                     GenState { freeBlocks = [], activeBlocks = M.empty, collections = 0 } 

test3 ::  Property
test3 = let ((ptr,gcS),_) = runTest (allocGen0 12) start emptyAllocM 
        in True ==> ptr == int2Ptr 0xc && (freeBlocks . head . generations) gcS == [] 
    where aBlock = Block { beginPtr = 0x0, endPtr = 0x400, freePtr = 0xc }
          start = mkGcState  
                     GenState { freeBlocks = [aBlock], activeBlocks = M.empty, collections = 0 } 

test4 ::  Property
test4 = let ((ptr,gcS),_) = runTest (allocGen0 12) start emptyAllocM 
        in True ==> ptr == int2Ptr 0x401 && (freeBlocks . head . generations) gcS == [] 
    where aBlock = Block { beginPtr = 0x0, endPtr = 0x400, freePtr = 0x400 }
          aBlock2 = Block { beginPtr = 0x401, endPtr = 0x800, freePtr = 0x401 }
          active' = M.insert (freeSpace aBlock) [aBlock] M.empty
          active'' = M.insert (freeSpace aBlock2) [aBlock2] active'
          start = mkGcState  
                     GenState { freeBlocks = [], activeBlocks = active'', collections = 0 } 

test5 ::  Int -> Property
test5 s = let ((ptr,gcS),_) = runTest (allocGen0 s) start AllocM { freeS = 0x801 }
          in s > 1 && s < blockSize ==> ptr == int2Ptr 0x801 && (length . M.toList . activeBlocks . head . generations) gcS == 3
    where aBlock = Block { beginPtr = 0x0, endPtr = 0x400, freePtr = 0x400 }
          aBlock2 = Block { beginPtr = 0x401, endPtr = 0x800, freePtr = 0x7FF }
          active' = M.insertWith (++) (freeSpace aBlock) [aBlock] M.empty
          active'' = M.insertWith (++) (freeSpace aBlock2) [aBlock2] active'
          start = mkGcState  
                     GenState { freeBlocks = [], activeBlocks = active'', collections = 0 } 

