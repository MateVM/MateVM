{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Compiler.Mate.Frontend.LivenessPass
  ( livenessPass
  , computeLiveRanges
  , printLiveRanges
  , LiveRanges
  ) where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

import Control.Applicative hiding ((<*>))
import Control.Monad.State
import Text.Printf

import Compiler.Hoopl
import Compiler.Mate.Frontend

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
    live (IRReturn _ (Just t)) _ = addVar t bot
    live (IRReturn _ _) _ = bot
    live (IRIfElse _ _ src1 src2 lt lf) f = addVar src1 $ addVar src2 $
                                            (factLabel f lt `S.union` factLabel f lf)
    live (IRJump lab) f = factLabel f lab
    live y _ = error $ "hoopl: livetransfer: not impl. yet: " ++ show y
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
    annotate (IRLabel _ l hm mh) f = retCO (IRLabel f l hm mh)
    annotate (IROp _ opt dst src1 src2) f = retOO (IROp f opt dst src1 src2)
    annotate (IRReturn _ ret) _ = retOC (IRReturn bot ret)
    annotate (IRJump _) _ = return Nothing
    annotate (IRExHandler _) _ = return Nothing
    annotate (IRIfElse _ jcmp src1 src2 l1 l2) f =
      retOC $ IRIfElse (factLabel f l1 `S.union` factLabel f l2) jcmp src1 src2 l1 l2
    annotate y _ = error $ "livenessAnnotate: not impl. yet: " ++ show y

    retCO :: forall m1. FuelMonad m1
          => (MateIR Var) C O
          -> m1 (Maybe (Graph (MateIR Var) C O))
    retCO = return . Just . mkFirst

    retOO :: forall m1. FuelMonad m1
          => (MateIR Var) O O
          -> m1 (Maybe (Graph (MateIR Var) O O))
    retOO = return . Just . mkMiddle

    retOC :: forall m1. FuelMonad m1
          => (MateIR Var) O C
          -> m1 (Maybe (Graph (MateIR Var) O C))
    retOC = return . Just . mkLast

    bot :: LiveSet
    bot = fact_bot livenessLattice
    factLabel :: FactBase LiveSet -> Label -> LiveSet
    factLabel f l = fromMaybe bot $ lookupFact l f

-- live ranges
-- TODO: limitation: one reg can just have one live range
type PC = Int
type LiveStart = M.Map PC [VirtualReg]
type LiveEnd = M.Map VirtualReg PC
type LiveRanges = (LiveStart, LiveEnd)

data LiveStateData = LiveStateData
  { active :: LiveSet
  , linstructions :: [LinearIns Var]
  , lstarts :: LiveStart
  , lends :: LiveEnd
  , pcCnt :: Int
  }

-- what we probably really need for LSRA:
-- (1) LiveStart = M.Map PC [VirtualReg]
-- (2) LiveEnd   = M.Map VirtualReg PC

type LiveState a = State LiveStateData a

computeLiveRanges :: [LinearIns Var] -> (LiveStart, LiveEnd)
computeLiveRanges insn = (ls, le)
  where
    endstate = snd $ runState step initstate
    ls = lstarts endstate
    le = lends endstate
    keys = M.keys $ lstarts endstate
    initstate = LiveStateData
                  { active = S.empty
                  , linstructions = insn
                  , lstarts = M.empty
                  , lends = M.empty
                  , pcCnt = 0
                  }

step :: LiveState ()
step = do
  insn <- linstructions <$> get
  when (not $ null insn) $ do
    pc <- pcCnt <$> get
    let (ins:insns) = insn

    la <- extractLiveAnnotation ins
    lsmap <- lstarts <$> get
    lemap <- lends <$> get
    cur <- active <$> get
    let newguys = S.toList $ la `S.difference` cur
    let deadguys = cur `S.difference` la

    let alt Nothing = Just newguys
        alt (Just old) = Just $ newguys ++ old
    modify (\s -> s { linstructions = insns
                    , active = la
                    , lstarts = M.alter alt pc lsmap
                    , lends   = S.fold (\k -> M.insert k pc) lemap deadguys
                    })
    incPC
    step

extractLiveAnnotation :: LinearIns Var -> LiveState LiveAnnotation
extractLiveAnnotation ins =
  case ins of
    Fst i -> case i of
      IRLabel la _ _ _  -> return la
    Mid i -> case i of
      IROp la _ _ _ _   -> return la
      IRStore la _ _ _  -> return la
      IRLoad la _ _ _   -> return la
      IRMisc1 la _ _    -> return la
      IRMisc2 la _ _ _  -> return la
      IRPrep _ _        -> active <$> get
      IRInvoke la _ _ _ -> return la
      IRPush la _ _     -> return la
    Lst i -> case i of
      IRJump _              -> active <$> get
      IRIfElse la _ _ _ _ _ -> return la
      IRExHandler _         -> active <$> get
      IRSwitch la _ _       -> return la
      IRReturn la _         -> return la

incPC :: LiveState ()
incPC = modify (\s -> s { pcCnt = 1 + (pcCnt s) })

printLiveRanges :: LiveRanges -> IO ()
printLiveRanges (ls, le) = do
  forM_ (M.keys ls) $ \frompc -> do
      forM_ (ls M.! frompc) $ \var -> do
        let topc = le M.! var
        printf "%6d: from %04d -> %04d active\n" var frompc topc
