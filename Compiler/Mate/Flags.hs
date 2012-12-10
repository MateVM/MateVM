{-# LANGUAGE OverloadedStrings #-}
module Compiler.Mate.Flags where

{-# INLINE usePreciseGC #-}
usePreciseGC :: Bool
usePreciseGC = False

{-# INLINE useBlockAllocator #-}
useBlockAllocator :: Bool
useBlockAllocator = False

-- objects bigger than this get allocated in LOH
loThreshhold :: Int
loThreshhold = 1024

useLoh :: Bool
useLoh = True

{-# INLINE enableCoalescing #-}
enableCoalescing :: Bool
enableCoalescing = False
