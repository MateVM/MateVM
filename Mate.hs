{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
#include "debug.h"
module Main where

import System.Environment
import Data.Char
import Data.List
import Data.List.Split
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
  parseArgs args False

parseArgs :: [String] -> Bool -> IO ()
parseArgs ("-jar":jarpath:_) stdcp = do
  if not stdcp then addClassPath "./" else return ()
  addClassPathJAR jarpath
  res <- readMainClass jarpath
  case res of
    Nothing -> error "JAR: no MainClass entry found. Try to pass the jar file via -cp instead."
    Just mc -> do
      let bclspath = B.pack $ map (fromIntegral . ord) mc
      cls <- getClassFile bclspath
      executeMain bclspath cls
parseArgs ("-cp":cps) cpset = parseArgs ("-classpath":cps) cpset
parseArgs ("-classpath":cps:xs) False = do
  let paths = splitOn ":" cps
  mapM_ addStuff paths
  parseArgs xs True
  where
  addStuff :: String -> IO ()
  addStuff x
    | ".jar" `isSuffixOf` x = addClassPathJAR x
    | otherwise = addClassPath $ x ++ "/"
parseArgs ("-classpath":xs) _ = parseArgs ("-":xs) True -- usage
parseArgs (('-':_):_) _ = error "Usage: mate [-cp|-classpath <cp1:cp2:..>] [<class-file> | -jar <jar-file>]"
-- first argument which isn't prefixed by '-' should be a class file
parseArgs (clspath:_) stdcp = do
  if not stdcp then addClassPath "./" else return ()
  let bclspath = B.pack $ map (fromIntegral . ord) clspath
  cls <- getClassFile bclspath
  executeMain bclspath cls
parseArgs _ _ = parseArgs ["-"] False


executeMain :: B.ByteString -> Class Direct -> IO ()
executeMain bclspath cls = do
  let methods = classMethods cls; methods :: [Method Direct]
  case find (\x -> methodName x == "main") methods of
    Just m -> do
      let mi = MethodInfo "main" bclspath $ methodSignature m
      hmap <- parseMethod cls "main" $ methodSignature m
      case hmap of
        Just hmap' -> do
          entry <- compileBB hmap' mi
          addMethodRef entry mi [bclspath]
#ifdef DEBUG
          printf "executing `main' now:\n"
#endif
          executeFuncPtr entry
        Nothing -> error "main not found"
    Nothing -> error "main not found"
