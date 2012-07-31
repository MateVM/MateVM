{-# LANGUAGE CPP #-}
module Mate.NativeMaschine(
  emitFromBB,
  mateHandler,
  register_signal,
  wordSize
  )where

#ifdef i386_HOST_ARCH
import Mate.X86CodeGen
import Mate.X86TrapHandling

wordSize :: Int
wordSize = 4

#else
#error "no other arch supported yet :/"
#endif
