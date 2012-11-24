{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Compiler.Mate.Frontend.PassOneUseDef
  ( oneUseDefPass
  , oudTransferID
  , oudTransfer
  , oudKill
  ) where

import qualified Data.Map as M

import Text.Printf

import Compiler.Hoopl
import Compiler.Mate.Frontend.IR

{- sample hoopl pass: usedef analysis -}
type OneUseDefFact = M.Map Var (WithTop Var)

oneUseDefLattice :: DataflowLattice OneUseDefFact
oneUseDefLattice = DataflowLattice
  { fact_name = "Use/Def Analysis"
  , fact_bot  = M.empty
  , fact_join = joinMaps (extendJoinDomain factAdd) }
  where
    factAdd _ (OldFact old) (NewFact new)
      | old == new = (NoChange, PElem new)
      | otherwise  = (SomeChange, Top)

oudTransfer :: BwdTransfer (MateIR Var) OneUseDefFact
oudTransfer = mkBTransfer3 usesCO usesOO usesOC
  where
    usesCO _ f = f
    usesOO (IROp Add dst@(VReg _ _) src@(VReg _ _) c) f=
      if c `elem` [JIntValue 0, JFloatValue 0, JRefNull]
        then if M.member src f
              -- more than one use, so don't look at it any more
              then M.insert src Top f
              else M.insert src (PElem dst) f
        else f
    usesOO (IRInvoke _ (Just r)) f = M.insert r Top f -- kill it...
    usesOO _ f = f
    usesOC (IRReturn _) _ = fact_bot oneUseDefLattice
    usesOC _ f = foldl (M.unionWith (\_ _ -> Top)) M.empty (mapElems f)

oudTransferID :: BwdTransfer (MateIR Var) OneUseDefFact
oudTransferID = mkBTransfer3 usesCO usesOO usesOC
  where
    usesCO _ f = f
    usesOO _ f = f
    usesOC _ f = foldl M.union M.empty (mapElems f)

oudKill :: forall m. FuelMonad m => BwdRewrite m (MateIR Var) OneUseDefFact
oudKill = mkBRewrite rw
  where
    rw ::    (MateIR Var) e x
          -> Fact x OneUseDefFact
          -> m (Maybe (Graph (MateIR Var) e x))
    rw ins@(IROp Add dst@(VReg _ _) src@(VReg _ _) c) f =
      let oprepl = if M.member dst f
                    then case f M.! dst of
                          PElem dstnew -> do
                            let newins = IROp Add dstnew src c
                            return $ Just $ mkMiddle $
                                   tracePipe (printf "rewrote1: \"%s\" to \"%s\"\n" (show ins) (show newins))
                                   newins
                          _ -> return Nothing
                    else return Nothing
      in if c == JIntValue 0 || c == JFloatValue 0
        then case M.lookup src f of
              Just Top -> oprepl
              Just _ -> return $ tracePipe (printf "killed: %s\n" (show ins)) $
                                 Just emptyGraph
              Nothing -> oprepl
        else oprepl
    rw ins@(IROp Add dst@(VReg _ _) c1 c2) f = case M.lookup dst f of
      Just (PElem newdst) -> do
        let newins = IROp Add newdst c1 c2
        return $ Just $ mkMiddle $
          tracePipe (printf "rewrote2: \"%s\" to \"%s\"\n" (show ins) (show newins))
          newins
      _ -> return Nothing
    rw ins@(IRLoadRT rt dst@(VReg _ _)) f = case M.lookup dst f of
      Just (PElem newdst) -> do
        let newins = IRLoadRT rt newdst
        return $ Just $ mkMiddle $
          tracePipe (printf "rewrote3: \"%s\" to \"%s\"\n" (show ins) (show newins))
          newins
      _ -> return Nothing
    rw _ _ = return Nothing

oneUseDefPass :: BwdPass SimpleFuelMonad (MateIR Var) OneUseDefFact
oneUseDefPass = BwdPass
  { bp_lattice = oneUseDefLattice
  , bp_transfer = oudTransfer
  , bp_rewrite = noBwdRewrite }
{- /sample pass -}
