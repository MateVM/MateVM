{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MateVM.Compiler.Mate.Runtime.GenerationalGC where

import Foreign
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Strict
import Control.Monad.Identity

import qualified Data.Map as M
import Data.Map(Map)

data Block = Block { beginPtr :: !IntPtr
                   , endPtr   :: !IntPtr
                   , freePtr  :: !IntPtr
                   } deriving Show

-- Maps number of free bytes to a set of blocks with this
-- amount of free memory
type Blocks = Map Int [Block]

data GenState = GenState { freeBlocks :: [Block]
                         , activeBlocks :: Blocks
                         , collections :: !Int 
                         } deriving Show

data GcState = GcState { generations :: [GenState] 
                       }

generation0 :: GcState -> GenState
generation0 = head . generations

class Monad m => Alloc a m | a -> m where
    alloc :: Int -> StateT a m Block 
    release :: Block -> StateT a m ()

type GenStateT m a = StateT GenState (StateT a m)
type GcStateT m a = StateT GcState (StateT a m) 

-- This is the mock allocator
data AllocM = AllocM { freeS :: IntPtr } deriving Show 
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
                 [] -> lift $ alloc size -- make a block
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
      else (intPtrToPtr freePtr', b { freePtr = freePtr' })
  where freePtr' = free' + fromIntegral size


-- | allocates memory in generation 0
allocGen0 :: Alloc a m => Int -> GcStateT m a (Ptr b)
allocGen0 size = do
    (gen0:xs) <- liftM generations get
    (ptr, newState) <- lift $ runStateT (allocGen size) gen0
    put (GcState $ newState:xs)
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
gcState1 = GcState [emptyGenState]


--dont be too frightened here. cornholio
runTest :: StateT GcState (StateT AllocM Identity) (Ptr a) -> GcState -> AllocM -> (Ptr a, AllocM)
runTest x gcState allocState = let allocation = evalStateT x gcState
                                   resultT = runStateT allocation allocState
                               in runIdentity resultT


test1 ::  (Ptr b, AllocM)
test1 = let x = evalStateT (allocGen0 12) gcState1
            y = runStateT x emptyAllocM
        in runIdentity y

test2 ::  IO (Ptr b, AllocIO)
test2 = let x = evalStateT (allocGen0 12) gcState1
            y = runStateT x AllocIO
        in y
