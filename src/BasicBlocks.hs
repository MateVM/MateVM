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
import JVM.Assembler

import Utilities

main = do
  args <- getArgs
  case args of
    [clspath] -> parseFile clspath
    _ -> error "Synopsis: dump-class File.class"


parseFile :: String -> IO ()
parseFile clspath = do
                     --clsFile <- decodeFile clspath
                     --putStrLn $ showListIx $ M.assocs $ constsPool (clsFile :: Class Pointers)
                     cls <- parseClassFile clspath
                     --dumpClass cls    
                     let mainmethod = lookupMethod "fib" cls -- "main|([Ljava/lang/String;)V" cf
                     putStrLn $ show $ testCFG mainmethod

test = parseFile "./tests/Fib.class"


testCFG :: Maybe (Method Resolved) -> String
testCFG (Just m) = case attrByName m "Code" of
                     Nothing       -> error "no code"
                     Just bytecode -> let code = decodeMethod bytecode
                                          instructions = codeInstructions code
                                      in show $ buildCFG instructions
testCFG _        = error "no method to build cfg"


buildCFG :: [Instruction] -> String
buildCFG xs = concatMap show xs
