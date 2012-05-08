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

printf_fake :: String -> (VarArgsFake t) => t
printf_fake _ = varFake []


-- see counterpart at `debug.h'
#ifndef DBG_JIT
printf_jit :: String -> (VarArgsFake t) => t
printf_jit = printf_fake
#endif

#ifndef DBG_BB
printf_bb :: String -> (VarArgsFake t) => t
printf_bb = printf_fake
#endif

#ifndef DBG_MP
printf_mp :: String -> (VarArgsFake t) => t
printf_mp = printf_fake
#endif

#ifndef DBG_CP
printf_cp :: String -> (VarArgsFake t) => t
printf_cp = printf_fake
#endif

#ifndef DBG_STR
printf_str :: String -> (VarArgsFake t) => t
printf_str = printf_fake
#endif
