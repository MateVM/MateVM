{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Mate.MethodPool where

import qualified Data.ByteString.Lazy as B

import Mate.Types
import Mate.NativeSizes
import Foreign.C.Types


addMethodRef :: (NativeWord, JpcNpcMap) -> MethodInfo -> [B.ByteString] -> IO ()
compileBB :: MethodInfo -> RawMethod -> MethodInfo -> IO (NativeWord, JpcNpcMap)
executeFuncPtr :: NativeWord -> IO ()
getMethodEntry :: MethodInfo -> IO (CPtrdiff, JpcNpcMap)
