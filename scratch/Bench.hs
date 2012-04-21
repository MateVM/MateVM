{-# LANGUAGE OverloadedStrings #-}
module Main where

import Frontend
import Graph
import Data.Maybe
import Control.Monad.State
import Data.Set as S
import Control.Monad

import Mate.BasicBlocks

import JVM.ClassFile
import JVM.Converter
import JVM.Assembler

import Mate.Utilities
import qualified Data.ByteString.Lazy as B

printG' :: Ord k => G k -> [k]
printG' g = evalState (foldGM ((: []), (++)) (\node -> let (Just v) = value node in v) [] g) S.empty

value (Node val _ _) = Just val
value (Leaf val    ) = Just val
value Nil            = Nothing


main = do con@(Just (ins,cls)) <- getMethodIO "../tests/AbsurdlyHuge.class" "absurdlyHuge"
          let perform' = do
                          let method = "absurdlyHuge"
                          let msig = methodSignature $ (classMethods cls) !! 1
                          --B.putStrLn (method `B.append` ": " `B.append` (encode msig))
                          let result = testCFG $ lookupMethod method cls
                          printMapBB result
          let perform = do
		  let tagged = getInstructions ins
		  let backRefs = splitBlocksBackRef tagged
		  let splitted = splitBlocks backRefs tagged
		  let transitions = getTransitions splitted
		  let nodes       = getNodes splitted
		  --print "nodes:"
		  --print nodes
		  --print "transitions"
		  --print transitions
		  let (Just finalCyclicStructure) = indirectCFGToG splitted
		  print "Final result"
		  print $ printG' finalCyclicStructure
          forM_ [0..100] (\x -> do perform)


