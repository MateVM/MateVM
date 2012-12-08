{-# LANGUAGE OverloadedStrings #-}
module Compiler.Mate.Flags where

{-# INLINE usePreciseGC #-}
usePreciseGC :: Bool
usePreciseGC = False

{-# INLINE useBlockAllocator #-}
useBlockAllocator :: Bool
useBlockAllocator = False

{-# INLINE enableCoalescing #-}
enableCoalescing :: Bool
enableCoalescing = False
