{-# LANGUAGE MultiParamTypeClasses #-}
module Compiler.Mate.Runtime.MockBlockAllocation where


import Foreign hiding ((.&.),unsafePerformIO)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Identity
import Test.QuickCheck hiding ((.&.))

import Text.Printf
import qualified Data.Map as M
import qualified Data.Set as S
import Compiler.Mate.Debug
import qualified Compiler.Mate.Runtime.GC as GC


import Compiler.Mate.Runtime.BlockAllocation

data AllocIO = AllocIO deriving Show
type AllocIOT a = StateT AllocIO IO a

-- This is the mock allocator
data AllocM = AllocM { freeS :: IntPtr } deriving (Eq)

emptyAllocM ::  AllocM
emptyAllocM = AllocM { freeS = 0 }
instance Show AllocM where
    show x = printf "freeS: 0x%08x" (fromIntegral $ freeS x :: Int)

instance Alloc AllocM Identity where
    alloc _ = mkBlockM
    release _ = return ()

type AllocMT a = StateT AllocM Identity a

instance Alloc AllocIO IO where
    alloc _ = mkBlockIO
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
  let block = Block { beginPtr = ptrToIntPtr ptr,
                      endPtr = ptrToIntPtr $ ptr `plusPtr` size,
                      freePtr = ptrToIntPtr ptr }  
  liftIO $ printfGc $ printf "made block: %s\n" (show block)
  return block
                        
releaseBlockIO :: Block -> AllocIOT ()
releaseBlockIO = liftIO . freeBlock
  where action = return . intPtrToPtr . beginPtr
        freeBlock = (freeDbg =<<) . action
        freeDbg ptr = do
                        printfGc $ printf "releaseBlock free ptr: %s" (show ptr)
                        free ptr

freeGen :: GenState -> AllocIOT ()
freeGen = mapM_ (mapM_ releaseBlockIO . snd) . M.toList . activeBlocks

freeGens :: [GenState] -> AllocIOT ()
freeGens = mapM_ freeGen 

freeGensIO :: [GenState] -> IO ()
freeGensIO xs = evalStateT (freeGens xs) AllocIO

blockAdresses :: Num a => a -> [(a,a)]
blockAdresses k = iterate next first
    where first = (0,k-1)
          next (l,u) = (l+k,u+k)

gcState1 ::  GcState
gcState1 = GcState { generations = M.insert 0 emptyGenState M.empty, allocs = 0, allocatedBytes = 0, loh = S.empty, 
                     allocState = error "not implemented" }


runBlockAllocator :: Int -> GcState -> IO (Ptr b, GcState)
runBlockAllocator size current = evalStateT allocT AllocIO
    where allocT = runStateT (allocGen0 GC.mkGen0 size) current


--dont be too frightened here. cornholio
runTest :: StateT GcState (StateT AllocM Identity) (Ptr a) -> GcState -> AllocM -> ((Ptr a, GcState), AllocM)
runTest x gcState allocState' = let allocation = runStateT x gcState
                                    resultT = runStateT allocation allocState'
                                    result = runIdentity resultT
                                in result


test1 ::  ((Ptr b, GcState), AllocM)
test1 = let x = runStateT (allocGen0 GC.mkGen0 12) gcState1
            y = runStateT x emptyAllocM
        in runIdentity y

test2 ::  IO ((Ptr b, GcState), AllocIO)
test2 = let x = runStateT (allocGen0 GC.mkGen0 12) gcState1
            y = runStateT x AllocIO
        in y

int2Ptr :: Int -> Ptr b
int2Ptr = intPtrToPtr . fromIntegral

emptyTest :: Int -> Property
emptyTest x = let ((ptr,_),_) = runTest (allocGen0 GC.mkGen0 x) start emptyAllocM 
              in x <= blockSize ==> ptr == int2Ptr 0
    where start = mkGcState  
                     GenState { freeBlocks = [], activeBlocks = M.empty, collections = 0, generation = 0} 
{-
test3 ::  Property
test3 = let ((ptr,gcS),_) = runTest (allocGen0 GC.mkGen0 12) start emptyAllocM 
        in True ==> ptr == int2Ptr 0xc && (freeBlocks . head . generations) gcS == [] 
    where aBlock = Block { beginPtr = 0x0, endPtr = 0x400, freePtr = 0xc }
          start = mkGcState  
                     GenState { freeBlocks = [aBlock], activeBlocks = M.empty, collections = 0, generation = 0 } 

test4 ::  Property
test4 = let ((ptr,gcS),_) = runTest (allocGen0 GC.mkGen0 12) start emptyAllocM 
        in True ==> ptr == int2Ptr 0x401 && (freeBlocks . head . generations) gcS == [] 
    where aBlock = Block { beginPtr = 0x0, endPtr = 0x400, freePtr = 0x400 }
          aBlock2 = Block { beginPtr = 0x401, endPtr = 0x800, freePtr = 0x401 }
          active' = M.insert (freeSpace aBlock) [aBlock] M.empty
          active'' = M.insert (freeSpace aBlock2) [aBlock2] active'
          start = mkGcState  
                     GenState { freeBlocks = [], activeBlocks = active'', collections = 0, generation = 0 } 

test5 ::  Int -> Property
test5 s = let ((ptr,gcS),_) = runTest (allocGen0 GC.mkGen0 s) start AllocM { freeS = 0x801 }
          in s > 1 && s < blockSize ==> ptr == int2Ptr 0x801 && (length . M.toList . activeBlocks . head . generations) gcS == 3
    where aBlock = Block { beginPtr = 0x0, endPtr = 0x400, freePtr = 0x400 }
          aBlock2 = Block { beginPtr = 0x401, endPtr = 0x800, freePtr = 0x7FF }
          active' = M.insertWith (++) (freeSpace aBlock) [aBlock] M.empty
          active'' = M.insertWith (++) (freeSpace aBlock2) [aBlock2] active'
          start = mkGcState  
                     GenState { freeBlocks = [], activeBlocks = active'', collections = 0, generation = 0 } 
-}
