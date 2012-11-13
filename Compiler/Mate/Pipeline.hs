{-# LANGUAGE OverloadedStrings #-}
module Compiler.Mate.Pipeline
  ( compileMethod
  ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntervalMap as IM
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Data.Int

import Harpy
import Harpy.X86Disassembler

import JVM.Assembler hiding (Instruction)
import qualified JVM.Assembler as J
import JVM.ClassFile
import JVM.Converter

import Compiler.Hoopl hiding (Label)

import Control.Monad.State
import Control.Applicative hiding ((<*>))
import Control.Arrow

import Text.Printf
import Compiler.Mate.Debug

import Compiler.Mate.Frontend
import Compiler.Mate.Backend
import Compiler.Mate.Backend.NativeSizes

import Compiler.Mate.Types
import Compiler.Mate.Utilities


pipeline :: Class Direct -> Method Direct -> [J.Instruction]
            -> IO (NativeWord, TrapMap)
pipeline cls meth jvminsn = do
    prettyHeader "JVM Input"
    mapM_ (printfPipe . printf "\t%s\n" . show) jvminsn
    prettyHeader "Hoopl Graph"
    printfPipe $ printf "%s\n" (showGraph show graph)
    -- prettyHeader "Label Map"
    -- printfPipe $ printf "%s\n" (show lbls)
    -- prettyHeader "Hoopl Opt-Graph"
    -- printfPipe $ printf "%s\n" (showGraph show optgraph)
    -- prettyHeader "Flatten Graph"
    -- printfPipe $ printf "%s\n" (show linear)
    prettyHeader "Register Allocation"
    printfPipe $ printf "%s\n" (show ra)
    prettyHeader "Code Generation"

    let cgconfig = defaultCodeGenConfig
                  { codeBufferSize = fromIntegral $ (length jvminsn) * 0x40 }
    let stinit = compileStateInit cls (methodName meth)
    (_, res) <- runCodeGenWithConfig (compileLinear lbls ra) () stinit cgconfig
    (dis, entry, trapmap) <- case res of
            Left err -> error $ "runCodeGen: " ++ show err
            Right (d, e, tm) -> return (d, e, tm)
    when mateDEBUG $ mapM_ (printfJit . printf "%s\n" . showIntel) dis
    printfJit $ printf "\n"
    return (entry, trapmap)
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
    ra = stupidRegAlloc (preRegs transstate) linear

prettyHeader :: String -> IO ()
prettyHeader str = do
  let len = length str + 6
  printfPipe $ printf "%s\n" (replicate len ' ')
  printfPipe $ printf "-- %s --\n" str
  printfPipe $ printf "%s\n" (replicate len ' ')

compileMethod :: B.ByteString -> MethodSignature -> Class Direct -> IO (NativeWord, TrapMap)
compileMethod meth sig cls = do
  case lookupMethodWithSig meth sig cls of
    Just m -> do
      let c = codeInstructions $ decodeMethod $ fromMaybe (error "no code seg") (attrByName m "Code")
      pipeline cls m c
    Nothing -> error $ "lookupMethod: " ++ show meth
{- /application -}
