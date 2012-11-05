module Compiler.Mate.Pipeline where

import qualified Data.ByteString.Lazy as B

import JVM.ClassFile
import Compiler.Mate.Types
import Compiler.Mate.Backend.NativeSizes

compileMethod :: B.ByteString -> MethodSignature -> Class Direct -> IO (NativeWord, TrapMap)
