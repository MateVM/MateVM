{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
module Compiler.Mate.Frontend.RegisterAllocation
  ( preeax
  , prexmm7
  , preFloats
  , preArgs
  , stupidRegAlloc
  , lsraMapping
  , noLiveRangeCollision
  , testLSRA
  , printMapping
  ) where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Word
import Data.Maybe
import Data.Ord

import Control.Applicative
import Control.Monad.State.Strict

import Test.QuickCheck hiding (labels)

import Text.Printf

import Harpy hiding (Label, fst, not, and)

import Compiler.Mate.Frontend.IR
import Compiler.Mate.Frontend.Linear
import Compiler.Mate.Frontend.LivenessPass

import Compiler.Mate.Backend.NativeSizes

-- import Debug.Trace

data MappedRegs = MappedRegs
  { regMap :: RegMapping
  , pcCounter :: Int
  , spillOffset :: Word32 }

{- pre assign hardware registers -}
preeax, prexmm7, preArgsLength, preArgsStart :: Num a => a
preeax = 99999
prexmm7 = 100000
preArgsLength = 6
preArgsStart = 200000
preArgs :: (Num a, Enum a) => [a]
preArgs = [preArgsStart .. (preArgsStart + preArgsLength - 1)]

preAssignedRegs :: RegMapping
preAssignedRegs = M.fromList
                  [ (VR preeax JInt,  HIReg eax)
                  , (VR prexmm7 JRef, HFReg xmm7)
                  ]

-- calling convention for floats is different: arguments are passed via xmm
-- registers, while int arguements are passed via stack slots

preFloatStart :: Num a => a
preFloatStart = 300000
preFloats :: (Num a, Enum a) => [a]
preFloats = [preFloatStart .. (preFloatStart + 5)]

{- the slots before are reservered for spilling registers on
 - GCPoints, see `saveReg' in codegen -}
stackOffsetStart :: Word32
stackOffsetStart = 0xffffffe8

emptyRegs :: MappedRegs
emptyRegs = MappedRegs preAssignedRegs 0 0

allIntRegs, allFloatRegs :: S.Set HVarX86
-- register usage:
-- - eax as scratch/int return
-- - esp/ebp for stack (TODO: maybe we can elimate ebp usage?)
-- - xmm7 as scratch/float return
allIntRegs = S.fromList $ map HIReg [ecx, edx, ebx, esi, edi]
allFloatRegs = S.fromList $ map HFReg [xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6]

stupidRegAlloc :: RegMapping -> PCActiveMap -> [LinearIns Var] -> Word32
               -> ([LinearIns HVarX86], Word32)
stupidRegAlloc preAssigned pcactive linsn stackcnt =
    (reslin, negate $ spillOffset stateres)
  where
    (reslin, stateres) = runState regAlloc' startmapping
    startassign = regMap emptyRegs `M.union` preAssigned
    startmapping = emptyRegs { regMap = startassign, spillOffset = stackcnt }
    regAlloc' = forM linsn $ \x -> do
                    r <- assignReg x
                    pcInc
                    return r

    pcInc :: State MappedRegs ()
    pcInc = modify (\s -> s { pcCounter = 1 + pcCounter s })

    pointMapping :: State MappedRegs [(HVarX86, VarType)]
    pointMapping = do
      pc <- pcCounter <$> get
      rm <- regMap <$> get
      let actives = S.toList (pcactive M.! pc)
      forM actives $ \vreg -> do
        unless (M.member vreg rm) $ do
          error "regalloc: should not happen @ point mapping"
        return (rm M.! vreg, vrTyp vreg)

    assignReg :: LinearIns Var -> State MappedRegs (LinearIns HVarX86)
    assignReg lv = case lv of
      Fst ins -> assignReg' ins Fst
      Mid ins -> case ins of
        IRPrep typ _ -> do
          ru <- pointMapping
          return $ Mid $ IRPrep typ ru
        _ -> assignReg' ins Mid
      Lst ins -> assignReg' ins Lst

    assignReg' :: MateIR Var e x
               -> (MateIR HVarX86 e x -> LinearIns HVarX86)
               -> State MappedRegs (LinearIns HVarX86)
    assignReg' ins blocktype = do
      assigns <- forM (varsIR ins) $ \x -> do
        y <- doAssign x; return (x, y)
      pm <- pointMapping
      let f = (M.!) (M.fromList assigns)
      return $ blocktype $ mapIR f pm ins

    doAssign :: Var -> State MappedRegs HVarX86
    doAssign (JIntValue x) = return $ HIConstant x
    doAssign JRefNull = return $ HIConstant 0
    doAssign (JFloatValue x) = return $ HFConstant x
    doAssign vr = do
      isAssignVr <- hasAssign vr
      if isAssignVr
        then getAssign vr
        else insertAssign vr
      where
        hasAssign :: Var -> State MappedRegs Bool
        hasAssign (VReg vreg) = M.member vreg <$> regMap <$> get
        hasAssign x = error $ "hasAssign: " ++ show x

        getAssign :: Var -> State MappedRegs HVarX86
        getAssign (VReg vreg) = (M.! vreg) <$> regMap <$> get
        getAssign x = error $ "getAssign: " ++ show x

        -- if there's no assignment, fall back to stupid allocation again (and
        -- only use spill slots)
        insertAssign :: Var -> State MappedRegs HVarX86
        insertAssign (VReg vreg) = do
          disp <- (+(-ptrSize)) <$> spillOffset <$> get
          let hreg = SpillIReg (Disp disp)
          modify (\s -> s { spillOffset = disp
                          , regMap = M.insert vreg hreg (regMap s)})
          return hreg
        insertAssign x = error $ "insertAssign: " ++ show x

-- lsra
data LsraStateData = LsraStateData
  -- mapping of virtual to hardware register (or spills)
  { regmapping :: RegMapping
  -- set of non-used hardware registers
  , freeHRegs :: [HVarX86]
  -- current offset on stack (for simplicity, don't reuse spill slots)
  , stackDisp :: Word32
  -- set of active virtual registers
  , activeRegs :: S.Set VirtualReg
  -- on every program counter, we store the current active map, in order to have
  -- information on GC operations or method invocation
  , pc2active :: PCActiveMap }
type PCActiveMap = M.Map Int (S.Set VirtualReg)
type LsraState a = State LsraStateData a

lsraMapping :: RegMapping
            -> LiveRanges
            -> (RegMapping, Word32, PCActiveMap)
lsraMapping precolored (LiveRanges lstarts lends) =
    (regmapping mapping, stackDisp mapping, pc2active mapping)
  where
    lastPC = S.findMax $ M.keysSet lstarts
    mapping = execState lsra
              LsraStateData { regmapping = preAssignedRegs `M.union` precolored
                            , freeHRegs = S.toList allIntRegs
                            , stackDisp = stackOffsetStart
                            , pc2active = M.insert 0 S.empty M.empty
                            , activeRegs = S.empty }

    lsra :: State LsraStateData ()
    lsra = do
      forM_ [0 .. lastPC] $ \pc -> do
        case pc `M.lookup` lstarts of
          Nothing -> return () -- nothing todo at this program counter
          Just new -> do -- we have to assign new regs
            freeRegs pc -- first, free dead ones
            forM_ new $ \vreg -> do
              hasMapping <- M.member vreg <$> regmapping <$> get -- maybe pre assigned
              when (vreg /= (VR preeax JInt)) $
                modify (\s -> s { activeRegs = S.insert vreg (activeRegs s) })
              unless hasMapping $ do
                fr <- freeHRegs <$> get
                if null fr
                  then spillReg vreg -- no hardware register available
                  else do
                    let hreg = head fr
                    modify (\s -> s { freeHRegs = tail fr
                                    , regmapping = M.insert vreg hreg (regmapping s)
                                    })
        -- active set for each program counter, needed for gc points
        active <- activeRegs <$> get
        modify (\s -> s { pc2active = M.insert (pc + 1) active (pc2active s) })

    freeRegs :: Int -> LsraState ()
    freeRegs pc = do
      active <- activeRegs <$> get
      forM_ (S.toList active) $ \vreg ->
        when (M.member vreg lends) $ do
          -- TODO: test if `<=' works too
          when ((lends M.! vreg) < pc) $ do
            hreg <- (M.! vreg) <$> regmapping <$> get
            modify (\s -> s { activeRegs = S.delete vreg (activeRegs s) })
            unless (isSpill hreg) $ do
              modify (\s -> s { freeHRegs = hreg : freeHRegs s })

    spillReg :: VirtualReg -> LsraState()
    spillReg vreg = do
      sc <- stackDisp <$> get
      let spill = SpillIReg (Disp sc)
      modify (\s -> s { stackDisp = stackDisp s - ptrSize })

      -- get all active virtual registers, mapped to hardware registers
      l <- (S.toList <$> activeRegs <$> get) >>= filterM noVirSpill
      -- create a map with virtual reg as key and end of live range as value (short list)
      let allends = map (\x -> (x, lends M.! x)) l
      -- sort by live range end (descending)
      let maxlive = reverse (L.sortBy (comparing snd) allends)
      when (null maxlive) $ error "lsra: active can't be empty here."

      let (lastreg, lastpc) = head maxlive
      -- if the current vreg to be assigned, has a shorter live range as some
      -- already signed vreg to a hreg, swap assignment
      if lastpc > (lends M.! vreg)
        then do
          m <- regmapping <$> get
          let hreg = m M.! lastreg
          modify (\s -> s { regmapping = M.delete lastreg (regmapping s) })
          modify (\s -> s { regmapping = M.insert lastreg spill (regmapping s) })
          modify (\s -> s { regmapping = M.insert vreg hreg (regmapping s) })
        else do
          modify (\s -> s { regmapping = M.insert vreg spill (regmapping s) })

    isSpill (SpillIReg _) = True
    isSpill _ = False

    noVirSpill vr@(VR _ _) = do
      m <- regmapping <$> get
      case M.lookup vr m of
        Just x -> return (not (isSpill x))
        Nothing -> return True


noLiveRangeCollision :: LiveRanges -> RegMapping -> Bool
noLiveRangeCollision (LiveRanges lstarts lends) rmapping =
    and [ and
            [ noCollision intk (intervalOfVreg j)
            | j <- sameHReg k ]
        | k <- vregs , let intk = intervalOfVreg k ]
  where
    vregs = M.keys rmapping
    sameHReg :: VirtualReg -> [VirtualReg]
    sameHReg vreg = L.delete vreg $ M.keys $ M.filter (== hreg) rmapping
      where
        hreg = rmapping M.! vreg
    intervalOfVreg :: VirtualReg -> (Int, Int)
    intervalOfVreg vreg = (start, end)
      where
        start = fst $ fromJust $ L.find (L.elem vreg . snd) $ M.toList lstarts
        end = lends M.! vreg
    noCollision :: (Int, Int) -> (Int, Int) -> Bool
    noCollision (x1, x2) (y1, y2) =
      x2 > x1 && y2 > y1 &&
      (  x2 < y1
      || y2 < x1)

prop_noCollision :: LiveRanges -> Bool
prop_noCollision lr = noLiveRangeCollision lr res
  where
    (res, _, _) = lsraMapping M.empty lr

testLSRA :: IO Result
testLSRA = do
  putStrLn "quickcheck lsra..."
  quickCheckResult prop_noCollision

instance Arbitrary LiveRanges where
  arbitrary = do
    pcEnd <- choose (10, 700) :: Gen Int
    vRegs' <- choose (10, 1500) :: Gen VRegNR
    vRegs <- forM [0 .. vRegs'] $ \vreg -> do
      typ <- elements [JRef, JInt]
      return (VR vreg typ)
    intervals <- forM vRegs $ \vreg -> do
      istart <- choose (0, pcEnd - 1) :: Gen Int
      iend <- choose (istart + 1, pcEnd) :: Gen Int
      return (vreg, (istart, iend))
    return $ LiveRanges
              (foldr (\(vreg, (is, _)) m ->
                              case M.lookup is m of
                                Nothing -> M.insert is [vreg] m
                                Just l -> M.insert is (vreg:l) m
                     ) M.empty intervals)
              (foldr (\(vreg, (_, ie)) -> M.insert vreg ie) M.empty intervals)


printMapping :: RegMapping -> String
printMapping m =
  flip concatMap (M.keys m) $ \x ->
    printf "vreg %12s  -> %10s\n" (show x) (show $ m M.! x)
