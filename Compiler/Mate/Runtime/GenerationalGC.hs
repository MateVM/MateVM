{-# LANGUAGE MultiParamTypeClasses #-}
module MateVM.Compiler.Mate.Runtime.GenerationalGC where

import Foreign
import Foreign.Marshal.Alloc
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Strict
import Control.Monad.Identity

data Block = Block { beginPtr :: !IntPtr
                   , endPtr   :: !IntPtr
                   , freePtr  :: !IntPtr
                   } deriving Show

data GenState = GenState { freeBlocks :: [Block]
                         , activeBlocks :: [Block]
                         , collections :: !Int 
                         } deriving Show

class Alloc a m where
    alloc :: Int -> StateT a m Block 

-- This is the mock allocator
data AllocM = AllocM { freeS :: IntPtr } deriving Show 
type AllocMT a = StateT AllocM Identity a

emptyAllocM :: AllocM
emptyAllocM = AllocM { freeS = 0 }

instance Alloc AllocM Identity where
    alloc = mkBlockM

data AllocIO = AllocIO deriving Show
type AllocIOT a = StateT AllocIO IO a

instance Alloc AllocIO IO where
    alloc = mkBlockIO

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
                         
