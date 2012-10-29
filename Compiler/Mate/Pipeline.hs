{-# LANGUAGE OverloadedStrings #-}
module Compiler.Mate.Pipeline
  ( compileMethod
  ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Control.Applicative hiding ((<*>))

import Harpy
import Harpy.X86Disassembler

import JVM.Assembler hiding (Instruction)
import qualified JVM.Assembler as J
import JVM.ClassFile
import JVM.Converter

import Compiler.Hoopl hiding (Label)
import qualified Compiler.Hoopl as H

import Control.Monad.State

import Debug.Trace
import Text.Printf

import Compiler.Mate.Frontend
import Compiler.Mate.Backend
import Compiler.Mate.Backend.NativeSizes

pipeline :: Class Direct -> Method Direct -> [J.Instruction] -> Bool -> IO NativeWord
pipeline cls meth jvminsn debug = do
    when debug $ prettyHeader "JVM Input"
    when debug $ mapM_ (printf "\t%s\n" . show) jvminsn
    when debug $ prettyHeader "Hoopl Graph"
    when debug $ printf "%s\n" (showGraph show graph)
    when debug $ prettyHeader "Label Map"
    when debug $ printf "%s\n" (show lbls)
    when debug $ prettyHeader "Hoopl Opt-Graph"
    when debug $ printf "%s\n" (showGraph show optgraph)
    when debug $ prettyHeader "Flatten Graph"
    when debug $ printf "%s\n" (show linear)
    when debug $ prettyHeader "Register Allocation"
    when debug $ printf "%s\n" (show ra)
    prettyHeader "Code Generation"
    (_, res) <- runCodeGen (compileLinear lbls ra) () M.empty
    (dis, entry) <- case res of
            Left err -> error $ "runCodeGen: " ++ show err
            Right (d, e) -> return (d, e)
    mapM_ (printf "%s\n" . showIntel) dis
    return entry
  where
    initstate = LabelLookup { labels = M.empty
                            , blockEntries = S.empty
                            , simStack = SimStack [] 50000 cls meth []
                            , instructions = jvminsn
                            , pcOffset = 0 }
    -- transform = foldl (liftM2 (|*><*|)) (return emptyClosedGraph) mkBlocks
    runAll prog = (runSimpleUniqueMonad . runStateT prog) initstate
    (graph, transstate) = runAll $ do
      resolveReferences
      refs <- blockEntries <$> get
      trace (printf "refs: %s\n" (show refs)) $
        resetPC jvminsn
      gs <- mkBlocks
      let g = L.foldl' (|*><*|) emptyClosedGraph gs
      mkMethod g
    runFM :: SimpleFuelMonad a -> a
    runFM = runSimpleUniqueMonad . runWithFuel infiniteFuel
    runOpts g = runFM $ do
      let nothingc = NothingC :: MaybeC O H.Label
      (_, f, _) <- analyzeAndRewriteBwd
                     oneUseDefPass nothingc g noFacts
      (gm', _, _) <- analyzeAndRewriteBwd
                     oneUseDefPass { bp_transfer = oudTransferID
                                   , bp_rewrite = oudKill }
                      nothingc g f
      trace (printf "facts: %s\n" (show f)) $ return gm'
    optgraph = runOpts graph
    lbls = labels transstate
    linear = mkLinear optgraph
    ra = stupidRegAlloc (preRegs . simStack $ transstate) linear

prettyHeader :: String -> IO ()
prettyHeader str = do
  let len = length str + 6
  replicateM_ len (putChar '-'); putStrLn ""
  printf "-- %s --\n" str
  replicateM_ len (putChar '-'); putStrLn ""
  -- putStrLn "press any key to continue..." >> getChar
  return ()

compileMethod :: B.ByteString -> Class Direct -> Bool -> IO NativeWord
compileMethod meth cls debug = do
  case lookupMethod meth cls of
    Just m -> do
      let code = codeInstructions $ decodeMethod $ fromMaybe (error "no code seg") (attrByName m "Code")
      pipeline cls m code debug
    Nothing -> error $ "lookupMethod: " ++ show meth
{- /application -}
