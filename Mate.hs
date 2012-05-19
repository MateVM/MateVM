{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
#include "debug.h"
module Main where

import System.Environment
import Data.Char
import Data.List
import Data.String.Utils
import qualified Data.ByteString.Lazy as B

#ifdef DEBUG
import Text.Printf
#endif
import JVM.ClassFile
import Java.JAR

import Mate.BasicBlocks
import Mate.MethodPool
import Mate.Types
import Mate.ClassPool
import Mate.X86TrapHandling

main ::  IO ()
main = do
  args <- getArgs
  register_signal
  addClassPath "./"
  case args of
    [clspath] -> do
      let bclspath = B.pack $ map (fromIntegral . ord) clspath
      cls <- getClassFile bclspath
      executeMain bclspath cls
    ["-jar", jarpath] -> do
      addClassPathJAR jarpath
      res <- readMainClass jarpath
      case res of
        Nothing -> error "JAR: no MainClass entry found. Try to pass the jar file via -cp instead."
        Just mc -> do
          let mc' = replace "." "/" mc
          let bclspath = B.pack $ map (fromIntegral . ord) mc'
          cls <- getClassFile bclspath
          executeMain bclspath cls
    _ -> error "Usage: mate [<class-file> | -jar <jar-file>]"

executeMain :: B.ByteString -> Class Direct -> IO ()
executeMain bclspath cls = do
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
