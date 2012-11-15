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

import JVM.Assembler hiding (Instruction)
import qualified JVM.Assembler as J
import JVM.ClassFile
import JVM.Converter

import Compiler.Hoopl hiding (Label)

import Control.Monad.State
import Control.Arrow

import Text.Printf
import Compiler.Mate.Frontend
import Compiler.Mate.Types
import Compiler.Mate.Utilities


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
    prettyHeader "Register Allocation"
    printf "%s\n" (show ra)
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
    runFM :: SimpleUniqueMonad a -> a
    runFM = runSimpleUniqueMonad -- . runWithFuel infiniteFuel
    runOpts g = runFM $ do
      -- let nothingc = NothingC :: MaybeC O H.Label
      -- (_, f, _) <- analyzeAndRewriteBwd
      --                oneUseDefPass nothingc g noFacts
      -- (gm', _, _) <- analyzeAndRewriteBwd
      --                oneUseDefPass { bp_transfer = oudTransferID
      --                              , bp_rewrite = oudKill }
      --                 nothingc g f
      -- tracePipe (printf "facts: %s\n" (show f)) $ return gm'
      return g
    optgraph = runOpts graph
    lbls = labels transstate
    linear = mkLinear optgraph
    (ra, stackAlloc) = stupidRegAlloc (preRegs transstate) linear

prettyHeader :: String -> IO ()
prettyHeader str = do
  let len = length str + 6
  printf "%s\n" (replicate len ' ')
  printf "-- %s --\n" str
  printf "%s\n" (replicate len ' ')
