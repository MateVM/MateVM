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

blockSize :: Int
blockSize = 2048

{-# INLINE enableCoalescing #-}
enableCoalescing :: Bool
enableCoalescing = False
