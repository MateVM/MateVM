{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Mate.MethodPool where

import qualified Data.ByteString.Lazy as B

import Data.Word
import Mate.Types
import Mate.NativeSizes
import Foreign.C.Types


addMethodRef :: CompiledMethod -> MethodInfo -> [B.ByteString] -> IO ()
compileBB :: MethodInfo -> RawMethod -> MethodInfo -> IO CompiledMethod
executeFuncPtr :: NativeWord -> IO ()
getMethodEntry :: MethodInfo -> IO (CPtrdiff, ExceptionMap Word32)
