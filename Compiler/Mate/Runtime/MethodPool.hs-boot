{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Compiler.Mate.Runtime.MethodPool
  ( lookupMethodEntry
  , executeFuncPtr
  ) where

import Compiler.Mate.Types
import Compiler.Mate.Backend.NativeSizes
import Foreign.C.Types

lookupMethodEntry :: MethodInfo -> IO CPtrdiff
executeFuncPtr :: NativeWord -> IO ()
