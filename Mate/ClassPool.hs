{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Mate.ClassPool where

import Data.Binary
import Data.String.Utils
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B
import System.Plugins

import Text.Printf

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.StablePtr
import Foreign.Marshal.Alloc

import JVM.ClassFile
import JVM.Converter

import Harpy
import Harpy.X86Disassembler

import Mate.BasicBlocks
import Mate.Types
import Mate.Utilities


getClassFile :: B.ByteString -> IO (Class Resolved)
getClassFile path = do
  let rpath = toString $ path `B.append` ".class"
  parseClassFile rpath
