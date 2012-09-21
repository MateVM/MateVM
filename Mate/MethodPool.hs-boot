{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Mate.MethodPool where

import qualified Data.ByteString.Lazy as B

import Mate.Types
import Mate.NativeSizes
import Foreign.C.Types


addMethodRef :: CompiledMethod -> MethodInfo -> [B.ByteString] -> IO ()
compileBB :: RawMethod -> MethodInfo -> IO CompiledMethod
executeFuncPtr :: NativeWord -> IO ()
executeException :: B.ByteString -> IO CPtrdiff
getMethodEntry :: MethodInfo -> IO (CPtrdiff, ExceptionMap NativeWord)
