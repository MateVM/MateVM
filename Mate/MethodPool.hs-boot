{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Mate.MethodPool where

import Data.Binary
import qualified Data.ByteString.Lazy as B

import Mate.Types
import Foreign.C.Types


addMethodRef :: Word32 -> MethodInfo -> [B.ByteString] -> IO ()
compileBB :: RawMethod -> MethodInfo -> IO Word32
executeFuncPtr :: Word32 -> IO ()
getMethodEntry :: CPtrdiff -> CPtrdiff -> IO CPtrdiff
