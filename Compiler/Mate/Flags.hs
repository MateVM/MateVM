{-# LANGUAGE OverloadedStrings #-}
module Compiler.Mate.Flags where

{-# INLINE usePreciseGC #-}
usePreciseGC :: Bool
usePreciseGC = True

{-# INLINE useBlockAllocator #-}
useBlockAllocator :: Bool
useBlockAllocator = True

-- objects bigger than this get allocated in LOH
loThreshhold :: Int
loThreshhold = 2048

useLoh :: Bool
useLoh = True

blockSizePowerOfTwo :: Int
blockSizePowerOfTwo = 11 --2048

heapSize :: Int
heapSize = 8192 -- (*) blockSize

useCachedAlloc :: Bool
useCachedAlloc = True

{-# INLINE enableCoalescing #-}
enableCoalescing :: Bool
enableCoalescing = False
