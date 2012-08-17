module Mate.NativeSizes where

import Data.Word

ptrSize, longSize :: NativeWord
#ifdef i386_HOST_ARCH
ptrSize = 4
longSize = 8

type NativeWord = Word32
#endif
