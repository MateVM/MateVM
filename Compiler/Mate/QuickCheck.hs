{-# LANGUAGE OverloadedStrings #-}
module Main where

import Compiler.Mate.Frontend

main :: IO ()
main = do
  testLSRA
  putStrLn "QuickCheck done"
