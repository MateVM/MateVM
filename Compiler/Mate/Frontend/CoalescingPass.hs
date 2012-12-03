{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Compiler.Mate.Frontend.CoalescingPass
  ( coalBwdPass
  ) where

import Prelude hiding (lookup)

import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe

import Compiler.Hoopl
import Compiler.Mate.Frontend.IR
import Compiler.Mate.Frontend.RegisterAllocation
import Compiler.Mate.Debug
import Compiler.Mate.Flags

-- either map to useage-count or actual merged var
type CoalFact = M.Map Var (WithTop (Either Int Var))

{- there are three states of a Var:
 -  (1) Var is used at least once
 -  (2) Var is mapped to other Var (coalescing)
 -  (3) don't consider this Var (because the possible other Var is already -  merged)
 -}

coalLattice :: DataflowLattice CoalFact
coalLattice = DataflowLattice
  { fact_name = "Coalescing Pass"
  , fact_bot  = M.empty
  , fact_join = joinMaps (extendJoinDomain factAdd) }
  where
    factAdd _ (OldFact old) (NewFact new)
      | old == new = (NoChange, PElem new)
      -- TODO: if both are left, make new left instead of Top?
      | otherwise = (SomeChange, Top)

bot :: CoalFact
bot = fact_bot coalLattice

factLabel :: FactBase CoalFact -> Label -> CoalFact
factLabel f l = fromMaybe bot $ lookupFact l f

coalTransfer :: BwdTransfer (MateIR Var) CoalFact
coalTransfer = mkBTransfer3 usesCO usesOO usesOC
  where
    usesCO _ f = f

    usesOO (IROp Add dst@(VReg (VR dstnr _)) src@(VReg (VR srcnr _)) c) f =
      if c `elem` [JIntValue 0, JFloatValue 0, JRefNull]
         && (srcnr /= preeax)
         && (srcnr > 9999) -- don't consider local vars (TODO)
         -- don't consider block interfaces (TODO)
         && (not (srcnr >= 600000 && srcnr < 700000))
         && (not (dstnr >= 600000 && dstnr < 700000))
         && (not (dstnr >= 200000 && dstnr < 300000))
        then case M.lookup dst f of
              Nothing -> M.insert dst (PElem $ Right src) f
              Just (PElem (Left _)) -> M.insert dst (PElem $ Right src) f
              Just Top -> f
              Just _ -> M.insert dst Top f
        else f
    usesOO ins f = addFacts (useIR ins) f

    usesOC ins f = addFacts (useIR ins) $ L.foldl' M.union bot succs
      where
        succs = map (factLabel f) $ successors ins

    addFacts vars f = foldr (\k lmap ->
                                case M.lookup k lmap of
                                  Nothing -> M.insert k (PElem $ Left 1) lmap
                                  _ -> lmap
                            ) f vars


coalKill :: forall m. FuelMonad m => BwdRewrite m (MateIR Var) CoalFact
coalKill = mkBRewrite3 rwCO rwOO rwOC
  where
    rwCO ins f = rewrite ins f mkFirst

    rwOO ins@(IROp Add dst@(VReg _) (VReg _) c) f =
      if c `elem` [JIntValue 0, JFloatValue 0, JRefNull]
        then case M.lookup dst f of
              Just (PElem (Right _)) -> return $ Just emptyGraph
              _ -> rewrite ins f mkMiddle
        else rewrite ins f mkMiddle
    rwOO ins f = rewrite ins f mkMiddle

    rwOC ins f = rewrite ins facts mkLast
      where
        -- hack for return, take some random fact (the are all equal anyways)
        facts = fromMaybe bot (lookupFact (head . mapKeys $ f) f)

    rewrite ins fact cnstr = return $ Just $ cnstr $ mapIR (lookup fact) [] ins

    lookup fact var =
      case M.lookup var fact of
        Just (PElem (Right r)) -> r
        _ -> var


coalTransferID :: BwdTransfer (MateIR Var) CoalFact
coalTransferID = mkBTransfer3 usesCO usesOO usesOC
  where
    usesCO _ f = f
    usesOO _ f = f
    -- again, some hack to gain faster runtime
    usesOC _ f = fromMaybe bot (lookupFact (head . mapKeys $ f) f)

coalPass :: BwdPass SimpleFuelMonad (MateIR Var) CoalFact
coalPass = BwdPass
  { bp_lattice = coalLattice
  , bp_transfer = coalTransfer
  , bp_rewrite = noBwdRewrite }


coalBwdPass :: forall x.
               Fact x CoalFact ~ LabelMap CoalFact => -- dafuq?
               Graph (MateIR Var) O x ->
               SimpleFuelMonad (Graph (MateIR Var) O x)
coalBwdPass g = do
  let nothingc = NothingC :: MaybeC O Label
  if enableCoalescing
    then do
      (_, f, _) <- analyzeAndRewriteBwd
                   coalPass nothingc g noFacts
      (gopt, _, _) <- analyzeAndRewriteBwd
                      coalPass { bp_transfer = coalTransferID
                               , bp_rewrite = coalKill }
                      nothingc g f
      tracePipe (printf "facts of coal: %s\n" (show f)) $
        return gopt
    else return g
