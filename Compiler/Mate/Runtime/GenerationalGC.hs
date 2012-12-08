{-# OPTIONS_GHC -fno-warn-orphans #-}
module Compiler.Mate.Runtime.GenerationalGC where

import Foreign
import Control.Monad.State
import qualified Data.Map as M
import Data.Map(Map)

import Compiler.Mate.Runtime.BlockAllocation
import Compiler.Mate.Runtime.GC

instance AllocationManager GcState where
    initMemoryManager = initGen
    mallocBytesT = mallocBytesGen
    performCollection = collectGen
    heapSize = error "heap size in GenGC not implemented"
    validRef = error "valid ref in GenGC not implemented"

initGen :: Int -> IO GcState
initGen _ = return . GcState $ map (const generation) [0..2] 
    where generation = GenState { freeBlocks = [], 
                                  activeBlocks = M.empty,
                                  collections = 0 }

mallocBytesGen :: Int -> StateT GcState IO (Ptr b)
mallocBytesGen size = do
    current <- get
    undefined

{- dafuq? could not match type?? this is well typed !11!
runBlockAllocator2 :: Int -> GcState -> IO (Ptr b, GcState)
runBlockAllocator2 size current = evalStateT allocT AllocIO
    where allocT = runStateT (allocGen0 size) current
-}


collectGen :: (RefObj b) => Map b RefUpdateAction -> StateT GcState IO ()
collectGen = undefined
