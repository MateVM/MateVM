{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Compiler.Mate.Frontend.LivenessPass
  ( livenessPass
  ) where

import qualified Data.Set as S
import Data.Maybe

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
    live (IRLabel _ _ _ _) f = f
    live (IROp _ _ dst src1 src2) f = removeVar dst $ addVar src1 $ addVar src2 f
    live (IRReturn _ (Just t)) f = addVar t bot
    live (IRReturn _ _) f = bot
    live _ _ = error "hoopl: livetransfer: not impl. yet"
    {- todo
    live (IRStore _ rt dst src) f = rtVar rt $ removeVar dst $ addVar src f
    live (IRLoad  _ rt dst src) f = rtVar rt $ removeVar dst $ addVar src f
    live (IRMisc1 _ _ src) f = addVar src f
    live (IRMisc2 _ _ dst src) f = removeVar dst $ addVar src f
    live (IRPrep _ _) f = f
    -}

    rtVar :: RTPool Var -> LiveSet -> LiveSet
    rtVar (RTIndex v _) f = addVar v f
    rtVar _ f = f

    bot :: LiveSet
    bot = fact_bot livenessLattice
    factLabel :: FactBase LiveSet -> Label -> LiveSet
    factLabel f l = fromMaybe bot $ lookupFact l f

    addVar :: Var -> LiveSet -> LiveSet
    addVar (VReg _ nr) f = S.insert nr f
    addVar _ f = f
    removeVar :: Var -> LiveSet -> LiveSet
    removeVar (VReg _ nr) f = S.delete nr f
    removeVar _ f = f


livenessAnnotate :: forall m . FuelMonad m => BwdRewrite m (MateIR Var) LiveSet
livenessAnnotate = mkBRewrite annotate
  where
    annotate :: (MateIR Var) e x -> Fact x LiveSet -> m (Maybe (Graph (MateIR Var) e x))
    annotate (IRLabel _ l hm mh) f = retCO (IRLabel (la f) l hm mh)
    annotate (IROp _ opt dst src1 src2) f = retOO (IROp (la f) opt dst src1 src2)
    annotate (IRReturn _ ret) _ = retOC (IRReturn (la bot) ret)
    annotate _ _ = return Nothing

    retCO :: forall m. FuelMonad m
          => (MateIR Var) C O
          -> m (Maybe (Graph (MateIR Var) C O))
    retCO = return . Just . mkFirst

    retOO :: forall m. FuelMonad m
          => (MateIR Var) O O
          -> m (Maybe (Graph (MateIR Var) O O))
    retOO = return . Just . mkMiddle

    retOC :: forall m. FuelMonad m
          => (MateIR Var) O C
          -> m (Maybe (Graph (MateIR Var) O C))
    retOC = return . Just . mkLast

    la = LiveAnnotation

    bot :: LiveSet
    bot = fact_bot livenessLattice
    factLabel :: FactBase LiveSet -> Label -> LiveSet
    factLabel f l = fromMaybe bot $ lookupFact l f
