{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntervalMap as IM
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Data.Int
import Data.Word

import JVM.Assembler hiding (Instruction)
import qualified JVM.Assembler as J
import JVM.ClassFile
import JVM.Converter

import Harpy hiding (Label, fst, not, and)

import Compiler.Hoopl

import Control.Applicative
import Control.Monad.State
import Control.Arrow

import Test.QuickCheck hiding (labels)

import Text.Printf
import Compiler.Mate.Frontend
import Compiler.Mate.Frontend.LivenessPass
import Compiler.Mate.Types
import Compiler.Mate.Utilities

import Debug.Trace


main :: IO ()
main = do
  let path = "tests/RegAlloc1"
  let mname = "regTest"
  cls <- parseClassFile (path ++ ".class")
  case L.find ((==) mname . methodName) (classMethods cls) of
    Just m -> do
      let sig = methodSignature m
      case lookupMethodWithSig mname sig cls of
        Just md -> do
          let c = codeInstructions $ decodeMethod $ fromMaybe (error "no code seg") (attrByName m "Code")
          fakeline cls md c
        Nothing -> error "bailout: 2\n"
    Nothing -> error "bailout: 1\n"


fakeline :: Class Direct -> Method Direct -> [J.Instruction]
            -> IO ()
fakeline cls meth jvminsn = do
    prettyHeader "Hoopl Graph"
    printf "%s\n" (showGraph show graph)
    -- prettyHeader "Label Map"
    -- printf "%s\n" (show lbls)
    prettyHeader "Hoopl Opt-Graph"
    printf "%s\n" (showGraph show optgraph)
    -- prettyHeader "Flatten Graph"
    -- printf "%s\n" (show linear)
    prettyHeader "Live Ranges"
    printf "%s\n" (show (zip [(0 :: Int)..] linear))
    printLiveRanges liveranges
    prettyHeader "LSRA Result"
    printMapping lsramap
    printf "no collisions? %s\n" (show $ noLiveRangeCollision liveranges lsramap)
    -- prettyHeader "Register Allocation"
    -- printf "%s\n" (show ra)
    return ()
  where
    mname = methodName meth
    codeseg = fromMaybe
              (error $ "codeseg " ++ (show . toString) mname ++ " not found")
              (attrByName meth "Code")
    decoded = decodeMethod codeseg
    exmap :: ExceptionMap Int32
    exmap = L.foldl' f IM.empty $ codeExceptions decoded
      where
        f emap ce =
          if IM.member key emap
            -- build list in reverse order, since matching order is important
            then IM.adjust (++ [value]) key emap
            else IM.insert key [value] emap
            where
              -- decrement end by one to get correct ranges
              key = IM.ClosedInterval (fromIntegral $ eStartPC ce)
                                      (fromIntegral $ eEndPC ce - 1)
              value = (&&&) g (fromIntegral . eHandlerPC) ce
                where
                  g ce' = case eCatchType ce' of
                      0 -> B.empty
                      x -> buildClassID cls x
    hstarts :: S.Set Int32
    hstarts = S.fromList $ map (fromIntegral . eHandlerPC)
                         $ codeExceptions decoded
    initstate :: ParseState'
    initstate = ParseState' { labels = M.empty
                            , nextTargets = []
                            , blockInterfaces = M.empty
                            , blockEntries = S.empty

                            , pcOffset = 0
                            , stack = []
                            , regcnt = 50000
                            , classf = cls
                            , method = meth
                            , preRegs = []

                            , instructions = jvminsn
                            , exceptionMap = exmap
                            , handlerStarts = hstarts }
    runAll prog = (runSimpleUniqueMonad . runStateT prog) initstate
    (graph, transstate) = runAll $ do
      addExceptionBlocks
      resolveReferences
      resetPC jvminsn
      gs <- mkBlocks
      mkMethod $ L.foldl' (|*><*|) emptyClosedGraph gs
    runFM :: SimpleFuelMonad a -> a
    runFM = runSimpleUniqueMonad . runWithFuel infiniteFuel
    runOpts g = runFM $ do
      let nothingc = NothingC :: MaybeC O Label
      (g', _, _) <- analyzeAndRewriteBwd
                       livenessPass nothingc g noFacts
      return g'
      -- (_, f, _) <- analyzeAndRewriteBwd
      --                oneUseDefPass nothingc g noFacts
      -- (gm', _, _) <- analyzeAndRewriteBwd
      --                oneUseDefPass { bp_transfer = oudTransferID
      --                              , bp_rewrite = oudKill }
      --                 nothingc g f
      -- tracePipe (printf "facts: %s\n" (show f)) $ return gm'
    optgraph = runOpts graph
    lbls = labels transstate
    linear = mkLinear optgraph
    liveranges = computeLiveRanges linear
    preColored = M.fromList $ preRegs transstate
    lsramap = lsraMapping preColored liveranges
    (ra, stackAlloc) = stupidRegAlloc (M.toList lsramap) linear

-- lsra
type RegMapping = M.Map VirtualReg (HVarX86, VarType)

data LsraStateData = LsraStateData
  { pcCnt :: Int
  , regmapping :: RegMapping
  , freeRegs :: [HVarX86]
  , stackCnt :: Word32
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
                                              , regmapping = precolored
                                              , freeRegs = S.toList allIntRegs
                                              , stackCnt = stackOffsetStart
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
            fr' <- freeRegs <$> get
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
      sc <- stackCnt <$> get
      let spill = SpillIReg (Disp sc)
      modify (\s -> s { stackCnt = stackCnt s - 4
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
  sam <- sample' (arbitrary :: Gen LiveRanges)
  printLiveRanges $ head sam
  -- quickCheck prop_noCollision

instance Arbitrary LiveRanges where
  arbitrary = do
    return $ LiveRanges M.empty M.empty


printMapping :: RegMapping -> IO ()
printMapping m = do
  forM_ (M.keys m) $ \x -> do
    printf "vreg %6d  -> %10s\n" x (show $ m M.! x)

prettyHeader :: String -> IO ()
prettyHeader str = do
  let len = length str + 6
  printf "%s\n" (replicate len ' ')
  printf "-- %s --\n" str
  printf "%s\n" (replicate len ' ')
