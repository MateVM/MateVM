{-# LANGUAGE OverloadedStrings #-}
module Mate.BasicBlocks where

import Data.Binary
import Data.Int
import System.Environment
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B

import JVM.Common
import JVM.ClassFile
import JVM.Converter
import JVM.Dump
import JVM.Assembler

import Mate.Utilities

type Name       = String -- use "virtual register id" instead?
data Type       = JInt | JFloat -- add more
type Variable   = (Type,Name)

-- Represents a CFG node
data BasicBlock = BasicBlock {
                     inputs  :: [Variable],
                     outputs :: [Variable],
                     code    :: [Instruction] }

-- Represents a Control-Flow-Graph as
-- Adjacency list (add matrix representation if appropriate)
type CFList     = [(BasicBlock, [BasicBlock])]


main = do
  args <- getArgs
  case args of
    [clspath] -> parseFile clspath "fib" -- TODO
    _ -> error "Synopsis: dump-class File.class"


parseFile :: String -> B.ByteString -> IO ()
parseFile clspath method = do
                     --clsFile <- decodeFile clspath
                     --putStrLn $ showListIx $ M.assocs $ constsPool (clsFile :: Class Pointers)
                     cls <- parseClassFile clspath
                     --dumpClass cls    
                     let mainmethod = lookupMethod method cls -- "main|([Ljava/lang/String;)V" cf
                     mapM_ putStrLn (testCFG mainmethod)

test_01 = parseFile "./tests/Fib.class" "fib"
test_02 = parseFile "./tests/While.class" "f"
test_03 = parseFile "./tests/While.class" "g"


testCFG :: Maybe (Method Resolved) -> [String]
testCFG (Just m) = case attrByName m "Code" of
                     Nothing       -> error "no code"
                     Just bytecode -> let code = decodeMethod bytecode
                                          instructions = codeInstructions code
                                      in buildCFG instructions
testCFG _        = error "no method to build cfg"


buildCFG :: [Instruction] -> [String]
buildCFG xs = map (\(x,y) -> show x ++ ", " ++ show y) xs'
  where
  xs' = calculateInstructionOffset xs

type Offset = (Int, Maybe Int16) -- (offset in bytecode, offset to jump target)

calculateInstructionOffset :: [Instruction] -> [(Offset, Instruction)]
calculateInstructionOffset = cio' (0, Nothing)
  where
  newoffset :: Instruction -> Int -> Offset
  newoffset x off = (off + (fromIntegral $ B.length $ encodeInstructions [x]), Nothing)
  cio' :: Offset -> [Instruction] -> [(Offset, Instruction)]
  cio' _ [] = []
  -- TODO(bernhard): add more instruction with offset (IF_ACMP, JSR, ...)
  -- TODO(bernhard): beautiful code please (BCP)
  cio' (off,_) (x@(IF _ w16):xs) = ((off, Just $ fromIntegral w16), x):(cio' (newoffset x off) xs)
  cio' (off,_) (x@(IF_ICMP _ w16):xs) = ((off, Just $ fromIntegral w16), x):(cio' (newoffset x off) xs)
  cio' (off,_) (x@(GOTO w16):xs) = ((off, Just $ fromIntegral w16), x):(cio' (newoffset x off) xs)
  cio' (off,_) (x:xs) = ((off, Nothing), x):(cio' (newoffset x off) xs)
