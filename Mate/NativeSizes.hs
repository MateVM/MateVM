{-# LANGUAGE CPP #-}
module Mate.NativeSizes where

import Data.Word

ptrSize, longSize :: NativeWord
#if defined(ARCH_X86)
ptrSize = 4
longSize = 8

type NativeWord = Word32
#endif
