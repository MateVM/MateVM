{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
#include "debug.h"
module Main where

import System.Environment
import Data.Char
import Data.List
import qualified Data.ByteString.Lazy as B

#ifdef DEBUG
import Text.Printf
#endif
import JVM.ClassFile

import Mate.BasicBlocks
import Mate.MethodPool
import Mate.Types
import Mate.ClassPool
import Mate.X86TrapHandling

main ::  IO ()
main = do
  args <- getArgs
  register_signal
  case args of
    [clspath] -> do
      let bclspath = B.pack $ map (fromIntegral . ord) clspath
      cls <- getClassFile bclspath
      hmap <- parseMethod cls "main"
      case hmap of
        Just hmap' -> do
          let methods = classMethods cls; methods :: [Method Direct]
          let method = find (\x -> methodName x == "main") methods
          case method of
            Just m -> do
              let mi = MethodInfo "main" bclspath $ methodSignature m
              entry <- compileBB hmap' mi
              addMethodRef entry mi [bclspath]
#ifdef DEBUG
              printf "executing `main' now:\n"
#endif
              executeFuncPtr entry
            Nothing -> error "main not found"
        Nothing -> error "main not found"
    _ -> error "Usage: mate <class-file>"
