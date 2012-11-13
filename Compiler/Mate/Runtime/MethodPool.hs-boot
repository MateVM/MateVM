{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Compiler.Mate.Runtime.MethodPool where

import qualified Data.ByteString.Lazy as B

import Compiler.Mate.Types
import Compiler.Mate.Backend.NativeSizes
import Foreign.C.Types


addMethodRef :: NativeWord -> MethodInfo -> [B.ByteString] -> IO ()
compile :: MethodInfo -> IO NativeWord
executeFuncPtr :: NativeWord -> IO ()
getMethodEntry :: MethodInfo -> IO CPtrdiff
