{-# LANGUAGE OverloadedStrings #-}
module Compiler.Mate.Flags where

{-# INLINE usePreciseGC #-}
usePreciseGC :: Bool
usePreciseGC = False

{-# INLINE enableCoalescing #-}
enableCoalescing :: Bool
enableCoalescing = True
