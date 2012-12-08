{-# OPTIONS_GHC -fno-warn-orphans #-}
module Compiler.Mate.Runtime.GenerationalGC where

import Foreign
import Control.Monad.State
import qualified Data.Map as M
import Data.Map(Map)

import Compiler.Mate.Runtime.BlockAllocation
import Compiler.Mate.Runtime.GC

instance AllocationManager GenState where
    initMemoryManager = initGen
    mallocBytesT = mallocBytesGen
    performCollection = collectGen
    heapSize = error "heap size in GenGC not implemented"
    validRef = error "valid ref in GenGC not implemented"

initGen :: Int -> IO GenState
initGen = undefined

mallocBytesGen :: Int -> StateT GenState IO (Ptr b)
mallocBytesGen = undefined

collectGen :: (RefObj b) => Map b RefUpdateAction -> StateT GenState IO ()
collectGen = undefined
