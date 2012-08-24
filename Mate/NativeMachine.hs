{-# LANGUAGE CPP #-}
module Mate.NativeMachine(
  emitFromBB,
  mateHandler,
  register_signal,
  ptrSize, longSize,
  NativeWord
  )where

#ifdef ARCH_X86
import Mate.X86CodeGen
import Mate.X86TrapHandling
import Mate.NativeSizes

#else
#error "no other arch supported yet :/"
#endif
