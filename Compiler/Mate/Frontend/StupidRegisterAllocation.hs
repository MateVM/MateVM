{-# LANGUAGE GADTs #-}
module Compiler.Mate.Frontend.StupidRegisterAllocation
  ( preeax
  , prexmm7
  , preFloats
  , preArgs
  , stupidRegAlloc
  , ptrSize -- TODO...
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
import Control.Arrow

import Test.QuickCheck hiding (labels)

import Text.Printf

import Harpy hiding (Label, fst, not, and)

import Compiler.Mate.Frontend.IR
import Compiler.Mate.Frontend.Linear
import Compiler.Mate.Frontend.LivenessPass

data MappedRegs = MappedRegs
  { regMap :: RegMapping
  , stackCnt :: Word32 }

ptrSize :: Num a => a
ptrSize = 4

{- pre assign hardware registers -}
preeax, prexmm7, preArgsLength, preArgsStart :: Integer
preeax = 99999
prexmm7 = 100000
preArgsLength = 6
preArgsStart = 200000
preArgs :: [Integer]
preArgs = [preArgsStart .. (preArgsStart + preArgsLength - 1)]

preAssignedRegs :: RegMapping
preAssignedRegs = M.fromList $
                  [ (preeax,  (HIReg eax, JInt))
                  , (prexmm7, (HFReg xmm7, JFloat))
                  ]

-- calling convention for floats is different: arguments are passed via xmm
-- registers, while int arguements are passed via stack slots

preFloatStart :: Integer
preFloatStart = 300000
preFloats :: [Integer]
preFloats = [preFloatStart .. (preFloatStart + 5)]

{- the slots before are reservered for spilling registers on
 - GCPoints, see `saveReg' in codegen -}
stackOffsetStart :: Word32
stackOffsetStart = 0xffffffe8

emptyRegs :: MappedRegs
emptyRegs = MappedRegs preAssignedRegs stackOffsetStart

allIntRegs, allFloatRegs :: S.Set HVarX86
-- register usage:
-- - eax as scratch/int return
-- - esp/ebp for stack (TODO: maybe we can elimate ebp usage?)
-- - xmm7 as scratch/float return
allIntRegs = S.fromList $ map HIReg [ecx, edx, ebx, esi, edi]
allFloatRegs = S.fromList $ map HFReg [xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6]

stupidRegAlloc :: RegMapping
               -> [LinearIns Var]
               -> ([LinearIns HVarX86], Word32)
stupidRegAlloc preAssigned linsn = (fst $ runState regAlloc' startmapping, 0 - 0xfffffce8)
  where
    startassign = M.union (regMap emptyRegs) preAssigned
    startmapping = emptyRegs { regMap = startassign }
    regAlloc' = mapM assignReg linsn

    rtRepack :: RTPool Var -> State MappedRegs (RTPool HVarX86)
    rtRepack (RTPool w16) = return $ RTPool w16
    rtRepack (RTPoolCall w16 []) = do
      mapping <- M.elems <$> regMap <$> get
      return $ RTPoolCall w16 mapping
    rtRepack (RTPoolCall _ x) = do
      error $ "regalloc: rtpoolcall: mapping should be empty: " ++ show x
    rtRepack (RTArray w8 obj [] w32) = do
      mapping <- M.elems <$> regMap <$> get
      return $ RTArray w8 obj mapping w32
    rtRepack (RTArray _ _ x _) = do
      error $ "regalloc: rtArray: mapping should be empty: " ++ show x
    rtRepack (RTIndex vreg typ) = do
      newreg <- doAssign vreg
      return $ RTIndex newreg typ
    rtRepack RTNone = return RTNone

    assignReg :: LinearIns Var -> State MappedRegs (LinearIns HVarX86)
    assignReg lv = case lv of
      Fst x -> case x of
        IRLabel la x' y z -> return $ Fst $ IRLabel la x' y z
      Mid ins -> case ins of
        IROp la op dst src1 src2 -> do
          dstnew <- doAssign dst
          src1new <- doAssign src1
          src2new <- doAssign src2
          return $ Mid $ IROp la op dstnew src1new src2new
        IRStore la rt obj src -> do
          objnew <- doAssign obj
          srcnew <- doAssign src
          nrt <- rtRepack rt
          return $ Mid $ IRStore la nrt objnew srcnew
        IRLoad la rt obj dst -> do
          objnew <- doAssign obj
          dstnew <- doAssign dst
          nrt <- rtRepack rt
          return $ Mid $ IRLoad la nrt objnew dstnew
        IRMisc1 la jins src -> do
          srcnew <- doAssign src
          return $ Mid $ IRMisc1 la jins srcnew
        IRMisc2 la jins dst src -> do
          dstnew <- doAssign dst
          srcnew <- doAssign src
          return $ Mid $ IRMisc2 la jins dstnew srcnew
        IRPrep typ _ -> do
          let ru = allIntRegs -- TODO
          return $ Mid $ IRPrep typ ru
        IRPush la nr src -> do
          srcnew <- doAssign src
          return $ Mid $ IRPush la nr srcnew
        IRInvoke la rt (Just r) ct -> do
          rnew <- Just <$> doAssign r
          nrt <- rtRepack rt
          return $ Mid $ IRInvoke la nrt rnew ct
        IRInvoke la rt Nothing ct -> do
          nrt <- rtRepack rt
          return $ Mid $ IRInvoke la nrt Nothing ct
      Lst ins -> case ins of
        IRJump l -> return $ Lst $ IRJump l
        IRIfElse la jcmp cmp1 cmp2 l1 l2 -> do
          cmp1new <- doAssign cmp1
          cmp2new <- doAssign cmp2
          return $ Lst $ IRIfElse la jcmp cmp1new cmp2new l1 l2
        IRExHandler t -> return $ Lst $ IRExHandler t
        IRSwitch la reg t -> do
          regnew <- doAssign reg
          return $ Lst $ IRSwitch la regnew t
        IRReturn la (Just b) -> do
          bnew <- Just <$> doAssign b
          return $ Lst $ IRReturn la bnew
        IRReturn la Nothing -> return $ Lst $ IRReturn la Nothing

    doAssign :: Var -> State MappedRegs HVarX86
    doAssign (JIntValue x) = return $ HIConstant x
    doAssign JRefNull = return $ HIConstant 0
    doAssign (JFloatValue x) = return $ HFConstant x
    doAssign vr = do
      isAssignVr <- hasAssign vr
      if isAssignVr
        then getAssign vr
        else error $ "regalloc: doAssign: no reg mapping?! " ++ show vr
      where
        hasAssign :: Var -> State MappedRegs Bool
        hasAssign (VReg _ vreg) = M.member vreg <$> regMap <$> get
        hasAssign x = error $ "hasAssign: " ++ show x

        getAssign :: Var -> State MappedRegs HVarX86
        getAssign (VReg _ vreg) = fst <$> (M.! vreg) <$> regMap <$> get
        getAssign x = error $ "getAssign: " ++ show x

-- lsra
type RegMapping = M.Map VirtualReg (HVarX86, VarType)

data LsraStateData = LsraStateData
  { pcCnt :: Int
  , regmapping :: RegMapping
  , freeRegs :: [HVarX86]
  , stackDisp :: Word32
  , activeRegs :: [VirtualReg]
  }
type LsraState a = State LsraStateData a

lsraMapping :: RegMapping
            -> LiveRanges
            -> RegMapping
lsraMapping precolored (LiveRanges lstarts lends) = regmapping mapping
  where
    lastPC = S.findMax $ M.keysSet lstarts
    mapping = execState (lsra) (LsraStateData { pcCnt = 0
                                              , regmapping = preAssignedRegs `M.union` precolored
                                              , freeRegs = S.toList allIntRegs
                                              , stackDisp = stackOffsetStart
                                              , activeRegs = []
                                              })
    incPC :: LsraState ()
    incPC = modify (\s -> s { pcCnt = 1 + (pcCnt s) })
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
              when (not hasMapping) $ do
                fr <- freeRegs <$> get
                if null fr
                  then spillGuy vreg
                  else do
                    let hreg = head fr
                    modify (\s -> s { freeRegs = tail fr
                                    , activeRegs = vreg : (activeRegs s)
                                    , regmapping = M.insert vreg (hreg, JInt) (regmapping s)
                                    })
        incPC
        lsra
    freeGuys :: Int -> LsraState ()
    freeGuys pc = do
      active <- activeRegs <$> get
      forM_ active $ \vreg -> do
        -- TODO: test if `<=' works too
        when ((lends M.! vreg) < pc) $ do
          hreg <- fst <$> (M.! vreg) <$> regmapping <$> get
          modify (\s -> s { activeRegs = L.delete vreg (activeRegs s)
                          , freeRegs = hreg:(freeRegs s) })
    spillGuy :: VirtualReg -> LsraState()
    spillGuy vreg = do
      sc <- stackDisp <$> get
      let spill = SpillIReg (Disp sc)
      modify (\s -> s { stackDisp = stackDisp s - 4
                      , regmapping = M.insert vreg (spill, JInt) (regmapping s)
                      })


noLiveRangeCollision:: LiveRanges -> RegMapping -> Bool
noLiveRangeCollision (LiveRanges lstarts lends) rmapping =
    and [ and
            [ noCollision intk (intervalOfVreg j)
            | j <- (sameHReg k) ]
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
prop_noCollision lr = noLiveRangeCollision lr (lsraMapping M.empty lr)

testLSRA :: IO ()
testLSRA = do
  putStrLn "quickcheck lsra..."
  -- sam <- sample' (arbitrary :: Gen LiveRanges)
  -- printLiveRanges $ head sam
  quickCheck prop_noCollision

instance Arbitrary LiveRanges where
  arbitrary = do
    pcEnd <- choose (10, 100) :: Gen Int
    vRegs <- choose (10, 50) :: Gen VirtualReg
    intervals <- forM [0 .. vRegs] $ \vreg -> do
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
printMapping m = do
  (flip concatMap) (M.keys m) $ \x ->
    printf "vreg %6d  -> %10s\n" x (show $ m M.! x)
