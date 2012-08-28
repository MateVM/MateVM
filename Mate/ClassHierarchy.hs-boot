module Mate.ClassHierarchy
  ( isInstanceOf
  , addClassEntry
  ) where

import qualified Data.ByteString.Lazy as B
import Mate.NativeSizes

isInstanceOf :: NativeWord -> B.ByteString -> IO Bool
addClassEntry :: NativeWord -> NativeWord -> IO ()
