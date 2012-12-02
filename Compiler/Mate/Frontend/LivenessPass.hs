{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Compiler.Mate.Frontend.LivenessPass
  ( liveBwdPass
  , LiveMateIR(..)
  , computeLiveRanges
  , LiveRanges(..)
  ) where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

import Control.Applicative hiding ((<*>))
import Control.Monad.State
import Text.Printf

import Compiler.Hoopl
import Compiler.Mate.Frontend.IR

type LiveMateIR' e x = (LiveAnnotation, MateIR Var e x)
newtype LiveMateIR e x = LiveMateIR (LiveMateIR' e x) deriving Show

instance NonLocal LiveMateIR where
  entryLabel (LiveMateIR (_, ins)) = entryLabel ins
  successors (LiveMateIR (_, ins)) = successors ins

liveBwdPass :: forall x.
               Fact x LiveSet ~ LabelMap LiveSet =>
               Graph (MateIR Var) O x ->
               SimpleFuelMonad (Graph LiveMateIR O x)
liveBwdPass g = do
  let nothingc = NothingC :: MaybeC O Label
  let gg = mapGraph (\x -> LiveMateIR (liveAnnEmpty, x)) g
  (g', _, _) <- analyzeAndRewriteBwd
                livenessPass nothingc gg noFacts
  return g'

livenessPass :: BwdPass SimpleFuelMonad LiveMateIR LiveSet
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

unpacklr :: forall e x. LiveMateIR e x -> LiveMateIR' e x
unpacklr (LiveMateIR x) = x

livenessTransfer :: BwdTransfer LiveMateIR LiveSet
livenessTransfer = mkBTransfer3 (unpacker liveCO) (unpacker liveOO) (unpacker liveOC)
  where
    unpacker g ins f = g (unpacklr ins) f

    liveCO (_, ins) f = factMerge f (varsIR' ins)
    liveOO (_, ins) f = factMerge f (varsIR' ins)
    liveOC (_, ins) f = factMerge facts (varsIR' ins)
      where
        facts = foldl S.union bot (map (factLabel f) $ successors ins)

    addVar :: Var -> LiveSet -> LiveSet
    addVar (VReg v) f = S.insert v f
    addVar _ f = f

    removeVar :: Var -> LiveSet -> LiveSet
    removeVar (VReg v) f = S.delete v f
    removeVar _ f = f

    factMerge :: LiveSet -> ([Var], [Var]) -> LiveSet
    factMerge f (defs, uses) = foldr removeVar (foldr addVar f uses) defs


livenessAnnotate :: forall m . FuelMonad m => BwdRewrite m LiveMateIR LiveSet
livenessAnnotate = mkBRewrite3 (unpacker annotateCO) (unpacker annotateOO)
                               (unpacker annotateOC)
  where
    unpacker g ins f = g (unpacklr ins) f

    annotateCO :: LiveMateIR' C O -> Fact O LiveSet -> m (Maybe (Graph LiveMateIR C O))
    annotateCO (_, ins) f = retCO (f, ins)

    annotateOO :: LiveMateIR' O O -> Fact O LiveSet -> m (Maybe (Graph LiveMateIR O O))
    annotateOO (_, IROp opt dvreg{-@(VReg dst)-} src1 src2) f
      -- fu @ java. see ./tests/Exception13
      -- | not (dst `S.member` f) = return $ Just emptyGraph
      | otherwise = retOO (f, IROp opt dvreg src1 src2)
    annotateOO (_, ins) f = retOO (f, ins)

    annotateOC :: LiveMateIR' O C -> Fact C LiveSet -> m (Maybe (Graph LiveMateIR O C))
    annotateOC (_, IRReturn ret) _ = retOC (bot, IRReturn ret)
    annotateOC (_, IRJump _) _ = return Nothing
    annotateOC (_, IRExHandler _) _ = return Nothing
    annotateOC (_, IRIfElse jcmp src1 src2 l1 l2) f =
      retOC $ ((factLabel f l1 `S.union` factLabel f l2), IRIfElse jcmp src1 src2 l1 l2)
    annotateOC (_, IRSwitch src lbls) f =
      retOC $ ((foldl S.union S.empty (map (factLabel f . snd) lbls)), IRSwitch src lbls)

    retCO :: forall m1. FuelMonad m1
          => LiveMateIR' C O
          -> m1 (Maybe (Graph LiveMateIR C O))
    retCO = return . Just . mkFirst . LiveMateIR

    retOO :: forall m1. FuelMonad m1
          => LiveMateIR' O O
          -> m1 (Maybe (Graph LiveMateIR O O))
    retOO = return . Just . mkMiddle . LiveMateIR

    retOC :: forall m1. FuelMonad m1
          => LiveMateIR' O C
          -> m1 (Maybe (Graph LiveMateIR O C))
    retOC = return . Just . mkLast . LiveMateIR

bot :: LiveSet
bot = fact_bot livenessLattice
factLabel :: FactBase LiveSet -> Label -> LiveSet
factLabel f l = fromMaybe bot $ lookupFact l f

-- live ranges
-- TODO: limitation: one reg can just have one live range
type LiveStart = M.Map PC [VirtualReg]
type LiveEnd = M.Map VirtualReg PC
data LiveRanges = LiveRanges LiveStart LiveEnd

data LiveStateData = LiveStateData
  { active :: LiveSet
  , lannos :: [LiveAnnotation]
  , lstarts :: LiveStart
  , lends :: LiveEnd
  , pcCnt :: Int
  }

type LiveState a = State LiveStateData a

computeLiveRanges :: [LiveAnnotation] -> LiveRanges
computeLiveRanges liveannos = LiveRanges ls le
  where
    endstate = snd $ runState step initstate
    ls = lstarts endstate
    le = lends endstate
    initstate = LiveStateData
                  { active = S.empty
                  , lannos = liveannos
                  , lstarts = M.empty
                  , lends = M.empty
                  , pcCnt = 0
                  }

step :: LiveState ()
step = do
  annos <- lannos <$> get
  unless (null annos) $ do
    pc <- pcCnt <$> get
    let (la:las) = annos

    lsmap <- lstarts <$> get
    lemap <- lends <$> get
    cur <- active <$> get
    let newguys = S.toList $ la `S.difference` cur
    let deadguys = cur `S.difference` la

    let alt Nothing = Just newguys
        alt (Just old) = Just $ newguys ++ old
    modify (\s -> s { lannos = las
                    , active = la
                    , lstarts = M.alter alt pc lsmap
                    , lends   = S.fold (`M.insert` pc) lemap deadguys
                    })
    incPC
    step

incPC :: LiveState ()
incPC = modify (\s -> s { pcCnt = 1 + pcCnt s })

instance Show LiveRanges where
  show (LiveRanges ls le) =
    flip concatMap (M.keys ls) $ \frompc ->
        flip concatMap (ls M.! frompc) $ \var ->
          let topc = le M.! var in
          printf "%12s: from %04d -> %04d active\n" (show var) frompc topc
