{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Compiler.Mate.Frontend.Linear
  ( mkLinear
  , LinearIns(..)
  ) where

import Text.Printf

import Compiler.Hoopl
import Compiler.Mate.Frontend.IR
import Compiler.Mate.Frontend.LivenessPass

data LinearIns t
  = Fst (MateIR t C O)
  | Mid (MateIR t O O)
  | Lst (MateIR t O C)

instance Show (LinearIns t) where
  show (Fst n) = printf "%s\n" $ show n
  show (Mid n) = printf "%s\n" $ show n
  show (Lst n) = printf "%s\n" $ show n

mkLinear :: Graph LiveMateIR O x -> ([LiveAnnotation], [LinearIns Var])
mkLinear = foldr (join . lineariseBlock) ([], []) . postorder_dfs
  where
    join (x1, y1) (x2, y2) = (x1 ++ x2, y1 ++ y2)
    -- see compiler/Lambdachine/Grin/RegAlloc.hs
    -- lineariseBlock :: Block LiveMateIR C C -> ([LiveAnnotation, [LinearIns Var])
    lineariseBlock block = (las, linsn)
      where
        las =    mybe fst entry_ins
              ++ map (\(LiveMateIR (l, _)) -> l) middles
              ++ mybe fst tail_ins
        linsn =    mybe snd entry_ins
                ++ map (\(LiveMateIR (_, x)) -> Mid x) middles
                ++ mybe snd tail_ins

        (entry, middles', tailb) = blockSplitAny block
        middles = blockToList middles'
        entry_ins :: [(LiveAnnotation, LinearIns Var)]
        entry_ins = case entry of
                      JustC (LiveMateIR (ela, n)) -> [(ela, Fst n)]
                      NothingC -> []
        tail_ins :: [(LiveAnnotation, LinearIns Var)]
        tail_ins = case tailb of
                      JustC (LiveMateIR (tla, n)) -> [(tla, Lst n)]
                      NothingC -> []

        mybe :: forall a x. (a -> x) -> [a] -> [x]
        mybe _ [] = []
        mybe f [i] = [f i]
        mybe _ _ = error "linear: mybe: doesn't happen"
