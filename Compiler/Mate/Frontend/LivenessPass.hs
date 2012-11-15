{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Compiler.Mate.Frontend.LivenessPass
  ( livenessPass
  ) where

import qualified Data.Set as S

-- import Text.Printf

import Compiler.Hoopl
import Compiler.Mate.Frontend.IR

livenessPass :: BwdPass SimpleFuelMonad (MateIR Var) LiveSet
livenessPass = BwdPass
  { bp_lattice = livenessLattice
  , bp_transfer = undefined
  , bp_rewrite = noBwdRewrite }

type LiveSet = S.Set VirtualReg

livenessLattice :: DataflowLattice LiveSet
livenessLattice = DataflowLattice
  { fact_name = "Liveness Analysis"
  , fact_bot  = S.empty
  , fact_join = undefined }
