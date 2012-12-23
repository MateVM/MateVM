module Compiler.Mate.Runtime.RtsOptions where

{-# INLINE usePreciseGC #-}
usePreciseGC :: Bool
usePreciseGC = False

{-# INLINE useBlockAllocator #-}
useBlockAllocator :: Bool
useBlockAllocator = True

--- objects bigger than this get allocated in LOH
loThreshhold :: Int
loThreshhold = 2040

useLoh :: Bool
useLoh = True

-- | use this flag in combination with usePreciseGC to get
-- very fast object allocation (way faster than pure malloc)
-- but with one huge evil memory leak. However this might be
-- useful for pure JIT benchmarks.
useLeakingGCForJitBenches :: Bool
useLeakingGCForJitBenches = False

blockSizePowerOfTwo :: Int
blockSizePowerOfTwo = 10 

heapSize :: Int
heapSize = 32768 -- (*) blockSize



