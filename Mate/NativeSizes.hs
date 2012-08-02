module Mate.NativeSizes where

import Data.Word

ptrSize, longSize :: Word32
#ifdef i386_HOST_ARCH
ptrSize = 4
longSize = 8
#endif
