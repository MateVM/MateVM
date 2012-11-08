{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# LANGUAGE GADTs #-}
module Compiler.Mate.Frontend.Linear
  ( mkLinear
  , LinearIns(..)
  ) where

import Text.Printf

import Compiler.Hoopl
import Compiler.Mate.Frontend.IR

data LinearIns t
  = Fst (MateIR t C O)
  | Mid (MateIR t O O)
  | Lst (MateIR t O C)

instance Show (LinearIns t) where
  show (Fst n) = printf "%s\n" $ show n
  show (Mid n) = printf "%s\n" $ show n
  show (Lst n) = printf "%s\n" $ show n

{- flatten hoople graph -}
mkLinear :: Graph (MateIR Var) O x -> [LinearIns Var] -- [Block (MateIR Var) C C]
mkLinear = concatMap lineariseBlock . postorder_dfs
  where
    -- see compiler/Lambdachine/Grin/RegAlloc.hs
    -- lineariseBlock :: Block (MateIR Var) C C -> [LinearIns Var]
    lineariseBlock block = entry_ins
                           ++ map Mid (blockToList middles)
                           ++ tail_ins
      where
        (entry, middles, tailb) = blockSplitAny block
        entry_ins :: [LinearIns Var]
        entry_ins = case entry of JustC n -> [Fst n]; NothingC -> []
        tail_ins :: [LinearIns Var]
        tail_ins = case tailb of JustC n -> [Lst n]; NothingC -> []
{- /linear -}
