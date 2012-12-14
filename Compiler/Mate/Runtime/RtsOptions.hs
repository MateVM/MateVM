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

blockSizePowerOfTwo :: Int
blockSizePowerOfTwo = 10 

heapSize :: Int
heapSize = 32768 -- (*) blockSize



