{-# LANGUAGE OverloadedStrings #-}
module BasicBlocks where

import Data.Binary
import System.Environment
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B

import JVM.Common
import JVM.ClassFile
import JVM.Converter
import JVM.Dump

import Utilities

main = do
  args <- getArgs
  case args of
    [clspath] -> do
      clsFile <- decodeFile clspath
      putStrLn $ showListIx $ M.assocs $ constsPool (clsFile :: Class Pointers)
      cls <- parseClassFile clspath
      dumpClass cls    
      let mainmethod = lookupMethod "main" cls -- "main|([Ljava/lang/String;)V" cf
      testCFG mainmethod
      putStrLn "foo"
    _ -> error "Synopsis: dump-class File.class"

testCFG _ = undefined
