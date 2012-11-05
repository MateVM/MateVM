module Compiler.Mate.Runtime.ClassHierarchy
  ( isInstanceOf
  , addClassEntry
  , addInterfaceEntry
  ) where

import qualified Data.ByteString.Lazy as B
import Compiler.Mate.Backend.NativeSizes

isInstanceOf :: NativeWord -> B.ByteString -> IO Bool
addClassEntry :: NativeWord -> NativeWord -> [B.ByteString] -> IO ()
addInterfaceEntry :: B.ByteString -> [B.ByteString] -> IO ()
