{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Compiler.Mate.Runtime.MethodPool where

import qualified Data.ByteString.Lazy as B

import Compiler.Mate.Types
import Compiler.Mate.Backend.NativeSizes
import Foreign.C.Types


addMethodRef :: CompiledMethod -> MethodInfo -> [B.ByteString] -> IO ()
compile :: MethodInfo -> IO CompiledMethod
executeFuncPtr :: NativeWord -> IO ()
getMethodEntry :: MethodInfo -> IO (CPtrdiff, ExceptionMap NativeWord)
