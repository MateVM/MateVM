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
import Compiler.Mate.Debug

import Compiler.Mate.Frontend
import Compiler.Mate.Backend
import Compiler.Mate.Backend.NativeSizes

import Compiler.Mate.Types


pipeline :: Class Direct -> Method Direct -> [J.Instruction]
            -> IO (NativeWord, TrapMap)
pipeline cls meth jvminsn = do
    prettyHeader "JVM Input"
    mapM_ (printfPipe . printf "\t%s\n" . show) jvminsn
    prettyHeader "Hoopl Graph"
    printfPipe $ printf "%s\n" (showGraph show graph)
    prettyHeader "Label Map"
    printfPipe $ printf "%s\n" (show lbls)
    prettyHeader "Hoopl Opt-Graph"
    printfPipe $ printf "%s\n" (showGraph show optgraph)
    prettyHeader "Flatten Graph"
    printfPipe $ printf "%s\n" (show linear)
    prettyHeader "Register Allocation"
    printfPipe $ printf "%s\n" (show ra)
    prettyHeader "Code Generation"

    let cgconfig = defaultCodeGenConfig
                  { codeBufferSize = fromIntegral $ (length jvminsn) * 32 }
    (_, res) <- runCodeGenWithConfig (compileLinear lbls ra) () M.empty cgconfig
    (dis, entry, trapmap) <- case res of
            Left err -> error $ "runCodeGen: " ++ show err
            Right (d, e, tm) -> return (d, e, tm)
    when mateDEBUG $ mapM_ (printfJit . printf "%s\n" . showIntel) dis
    printfJit $ printf "\n"
    return (entry, trapmap)
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
  when mateDEBUG (do replicateM_ len (putChar '-'); putStrLn "")
  printfPipe $ printf "-- %s --\n" str
  when mateDEBUG (do replicateM_ len (putChar '-'); putStrLn "")
  -- putStrLn "press any key to continue..." >> getChar
  return ()

compileMethod :: B.ByteString -> Class Direct -> IO (NativeWord, TrapMap)
compileMethod meth cls = do
  case lookupMethod meth cls of
    Just m -> do
      let c = codeInstructions $ decodeMethod $ fromMaybe (error "no code seg") (attrByName m "Code")
      pipeline cls m c
    Nothing -> error $ "lookupMethod: " ++ show meth
{- /application -}
