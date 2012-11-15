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
  , bp_transfer = livenessTransfer
  , bp_rewrite = livenessAnnotate }

type LiveSet = S.Set VirtualReg

livenessLattice :: DataflowLattice LiveSet
livenessLattice = DataflowLattice
  { fact_name = "Liveness Analysis"
  , fact_bot  = S.empty
  , fact_join = factAdd }
    where
      factAdd _ (OldFact old) (NewFact new) =
          (changeIf (S.size merged > S.size old), merged)
        where
          merged = new `S.union` old

livenessTransfer :: BwdTransfer (MateIR Var) LiveSet
livenessTransfer = mkBTransfer live
  where
    live :: (MateIR Var) e x -> Fact x LiveSet -> LiveSet
    live (IRLabel _ _ _) f = f
    live (IROp _ _ dst src1 src2) f = removeVar dst $ addVar src1 $ addVar src2 f
    -- TODO: rtstuff
    live (IRStore _ _ dst src) f = removeVar dst $ addVar src f
    live (IRLoad _ _ dst src) f = removeVar dst $ addVar src f
    live (IRReturn _ (Just t)) f = addVar t $ fact_bot livenessLattice
    live (IRReturn _ _) f = fact_bot livenessLattice

    addVar :: Var -> LiveSet -> LiveSet
    addVar (VReg typ nr) f = S.insert (nr, typ) f
    addVar _ f = f

    removeVar :: Var -> LiveSet -> LiveSet
    removeVar (VReg typ nr) f = S.delete (nr, typ) f
    removeVar _ f = f

livenessAnnotate :: forall m . FuelMonad m => BwdRewrite m (MateIR Var) LiveSet
livenessAnnotate = mkBRewrite annotate
  where
    annotate :: (MateIR Var) e x -> Fact x LiveSet -> m (Maybe (Graph (MateIR Var) e x))
    annotate (IROp _ opt dst src1 src2) f = return $ Just $ mkMiddle
      (IROp (LiveAnnotation f) opt dst src1 src2)
    annotate _ _ = return Nothing
