{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment

import Text.Printf

import JVM.Converter
import JVM.Dump

import Mate.BasicBlocks
import Mate.X86CodeGen
import Mate.MethodPool

main ::  IO ()
main = do
  args <- getArgs
  register_signal
  initMethodPool
  case args of
    [clspath] -> do
      cls <- parseClassFile clspath
      dumpClass cls
      hmap <- parseMethod cls "main"
      printMapBB hmap
      case hmap of
        Just hmap' -> do
          entry <- compileBB hmap' "main"
          printf "executing `main' now:\n"
          executeFuncPtr entry
        Nothing -> error "main not found"
    _ -> error "Usage: mate <class-file>"
