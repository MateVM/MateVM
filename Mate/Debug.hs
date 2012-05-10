{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
#include "debug.h"

module Mate.Debug where

{- we cannot use `PrintfType' from Text.Printf, as it isn't exported.
 - so we implement a `VarArgsFake' here.
 - http://www.haskell.org/haskellwiki/Varargs -}
class VarArgsFake t where
  varFake :: [String] -> t

instance VarArgsFake (IO a) where
  varFake _ = return undefined

instance (Show a, VarArgsFake r) => VarArgsFake (a -> r) where
  varFake _ = \_ -> varFake []

printfFake :: String -> (VarArgsFake t) => t
printfFake _ = varFake []


-- see counterpart at `debug.h'
#ifndef DBG_JIT
printfJit :: String -> (VarArgsFake t) => t
printfJit = printfFake
#endif

#ifndef DBG_BB
printfBb :: String -> (VarArgsFake t) => t
printfBb = printfFake
#endif

#ifndef DBG_MP
printfMp :: String -> (VarArgsFake t) => t
printfMp = printfFake
#endif

#ifndef DBG_CP
printfCp :: String -> (VarArgsFake t) => t
printfCp = printfFake
#endif

#ifndef DBG_STR
printfStr :: String -> (VarArgsFake t) => t
printfStr = printfFake
#endif
