{-# LANGUAGE CPP #-}
module Mate.NativeSizes where

import Data.Word

ptrSize, longSize :: Num a => a
#if defined(ARCH_X86)
ptrSize = 4
longSize = 8

type NativeWord = Word32
#else
-- HACK, for ghc-mod ...
ptrSize = undefined
longSize = undefined
type NativeWord = Word32
#endif
