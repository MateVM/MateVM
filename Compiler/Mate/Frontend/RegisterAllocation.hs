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

import Control.Applicative
import Control.Monad.State

import Test.QuickCheck hiding (labels)

import Text.Printf

import Harpy hiding (Label, fst, not, and)

import Compiler.Mate.Frontend.IR
import Compiler.Mate.Frontend.Linear
import Compiler.Mate.Frontend.LivenessPass

import Compiler.Mate.Backend.NativeSizes

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
      return [ (rm M.! vreg, vrTyp vreg) | vreg <- pcactive M.! pc ]

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
      let f = (M.!) (M.fromList assigns)
      return $ blocktype $ mapIR f ins

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
  { pcCnt :: PC
  -- mapping of virtual to hardware register (or spills)
  , regmapping :: RegMapping
  -- set of non-used hardware registers
  , freeRegs :: [HVarX86]
  -- current offset on stack (for simplicity, don't reuse spill slots)
  , stackDisp :: Word32
  -- set of active virtual registers
  , activeRegs :: [VirtualReg]
  -- on every program counter, we store the current active map, in order to have
  -- information on GC operations or method invocation
  , pc2active :: PCActiveMap
  }
type PCActiveMap = M.Map Int [VirtualReg]
type LsraState a = State LsraStateData a

lsraMapping :: RegMapping
            -> LiveRanges
            -> (RegMapping, Word32, PCActiveMap)
lsraMapping precolored (LiveRanges lstarts lends) =
    (regmapping mapping, stackDisp mapping, pc2active mapping)
  where
    lastPC = S.findMax $ M.keysSet lstarts
    mapping = execState lsra
              LsraStateData { pcCnt = 0
                            , regmapping = preAssignedRegs `M.union` precolored
                            , freeRegs = S.toList allIntRegs
                            , stackDisp = stackOffsetStart
                            , pc2active = M.empty
                            , activeRegs = [] }
    incPC :: LsraState ()
    incPC = modify (\s -> s { pcCnt = 1 + pcCnt s })
    lsra = do
      pc <- pcCnt <$> get
      -- when (trace (printf "pc: %d" pc) (pc <= lastPC)) $ do
      when (pc <= lastPC) $ do
        case pc `M.lookup` lstarts of
          Nothing -> return ()
          Just new -> do
            -- fr' <- freeRegs <$> get
            -- trace (printf "new: %s\nfree: %s" (show new) (show fr')) $
            freeGuys pc
            forM_ new $ \vreg -> do
              hasMapping <- M.member vreg <$> regmapping <$> get -- maybe pre assigned
              unless hasMapping $ do
                fr <- freeRegs <$> get
                if null fr
                  then spillGuy vreg
                  else do
                    let hreg = head fr
                    modify (\s -> s { freeRegs = tail fr
                                    , activeRegs = vreg : activeRegs s
                                    , regmapping = M.insert vreg hreg (regmapping s)
                                    })
        active <- activeRegs <$> get
        modify (\s -> s { pc2active = M.insert pc active (pc2active s) })
        incPC
        lsra
    freeGuys :: Int -> LsraState ()
    freeGuys pc = do
      active <- activeRegs <$> get
      forM_ active $ \vreg ->
        -- TODO: test if `<=' works too
        when ((lends M.! vreg) < pc) $ do
          hreg <- (M.! vreg) <$> regmapping <$> get
          modify (\s -> s { activeRegs = L.delete vreg (activeRegs s)
                          , freeRegs = hreg : freeRegs s })
    spillGuy :: VirtualReg -> LsraState()
    spillGuy vreg = do
      sc <- stackDisp <$> get
      let spill = SpillIReg (Disp sc)
      modify (\s -> s { stackDisp = stackDisp s - ptrSize
                      , regmapping = M.insert vreg spill (regmapping s)
                      })


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
