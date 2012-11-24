{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Exit
import Test.QuickCheck
import Compiler.Mate.Frontend

main :: IO ()
main = do
  res <- testLSRA
  case res of
    Success{} -> putStrLn "QuickCheck done"
    _ -> exitFailure
