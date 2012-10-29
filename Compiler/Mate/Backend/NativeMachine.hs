{-# LANGUAGE CPP #-}
module Compiler.Mate.Backend.NativeMachine(
  emitFromBB,
  mateHandler,
  register_signal,
  ptrSize, longSize,
  NativeWord
  )where

#ifdef ARCH_X86
import Compiler.Mate.Backend.X86CodeGenerator
import Compiler.Mate.Backend.X86TrapHandling
import Compiler.Mate.Backend.NativeSizes
#else

-- HACK, for ghc-mod ...
import Compiler.Mate.Backend.X86CodeGenerator
import Compiler.Mate.Backend.X86TrapHandling
import Compiler.Mate.Backend.NativeSizes
#endif
