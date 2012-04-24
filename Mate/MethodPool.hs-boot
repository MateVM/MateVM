{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Mate.MethodPool where

import Data.Binary
import qualified Data.ByteString.Lazy as B

import Mate.Types


addMethodRef :: Word32 -> MethodInfo -> [B.ByteString] -> IO ()
compileBB :: MapBB -> MethodInfo -> IO Word32
executeFuncPtr :: Word32 -> IO ()
