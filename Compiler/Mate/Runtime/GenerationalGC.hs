{-# OPTIONS_GHC -fno-warn-orphans #-}
module Compiler.Mate.Runtime.GenerationalGC where

import Foreign
import Control.Monad.State
import qualified Data.Map as M
import Data.Map(Map)

import Compiler.Mate.Runtime.BlockAllocation
import Compiler.Mate.Runtime.GC
import Compiler.Mate.Debug

maxGen :: Int
maxGen = 2 -- means 0,1,2

instance AllocationManager GcState where
    initMemoryManager = initGen
    mallocBytesT = mallocBytesGen
    performCollection = collectGen
    collectLoh = error "not implemented yet"
    heapSize = error "heap size in GenGC not implemented"
    validRef = error "valid ref in GenGC not implemented"

initGen :: Int -> IO GcState
initGen _ = return  GcState { generations = map (const generation) [0..maxGen],
                              allocs = 0,
                              allocatedBytes = 0 }
    where generation = GenState { freeBlocks = [], 
                                  activeBlocks = M.empty,
                                  collections = 0 }

logGen :: Bool
logGen = True

logGenT :: String -> StateT GcState IO ()
logGenT s = if logGen 
             then liftIO $ printfGc s 
             else return () 

mallocBytesGen :: Int -> StateT GcState IO (Ptr b)
mallocBytesGen size' = do
    current <- get

    (ptr,current') <- liftIO $ runBlockAllocator size' current 
    put $ current' { allocs = 1 + allocs current' }
    return ptr

-- given an element in generation x -> where to evaucuate to
sourceGenToTargetGen :: Int -> Int 
sourceGenToTargetGen 0 = 1
sourceGenToTargetGen 1 = 2
sourceGenToTargetGen 2 = 2
sourceGenToTargetGen x = error $ "source object is in strange generation: " ++ show x

collectGen :: (RefObj b) => Map b RefUpdateAction -> StateT GcState IO ()
collectGen = error "collect genGC"
