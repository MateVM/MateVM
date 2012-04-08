{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Printf

import Mate.X86CodeGen
import Mate.MethodPool

main ::  IO ()
main = do
  printf "fib Codegen:\n"
  test_01
  printf "\n\n\n\nData.Map & FFI:\n"
  t_01
