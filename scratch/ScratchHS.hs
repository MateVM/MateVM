{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Purpose of this file is just do test some Intermediate representations and stuff ;-)

{- Some important material:
 - 
 - Java HotSpot™ Client Compiler: www.cdl.uni-saarland.de/ssasem/talks/Christian.Wimmer.pdf
 - http://www.complang.tuwien.ac.at/andi/185A50
 - 
 - [Poletto:1999] http://dl.acm.org/citation.cfm?doid=330249.330250
 - [Wimmer:2010] http://dl.acm.org/citation.cfm?id=1772954.1772979
 -
 -}


module ScratchHS where

import Data.Maybe
import Control.Monad.State

import Harpy hiding(fst,add)
import qualified Harpy.X86Disassembler as H
import qualified Data.ByteString.Lazy as B
import qualified Data.Set as S
import qualified Data.Heap as H

import Foreign

import Debug.Trace
import Data.Int

import JVM.ClassFile
import JVM.Converter
import JVM.Dump

import JVM.Assembler 

import Mate.Utilities
import Mate.BasicBlocks

import Frontend
import Graph

$(callDecl "callAsWord32" [t|Word32|])

data SimpleStack = PushLit Int
                 | Mul
                 | Add
                 | Ld String
                 | Print

testP = [PushLit 3, PushLit 2, Mul]

type Reg = Int 
data ROp = RMul | RAdd

data RegIL = RMov Reg Reg
           | RLoad Reg String 
           | RBin  Reg Reg Reg ROp

data MateState = MateState String

compileRegIL :: RegIL -> CodeGen (Ptr Int32) MateState ()
compileRegIL (RMov t s) = do 
                           mateState <- getState
                           let (mt,ms) = (eax,eax)
                           mov mt ms


entryCode :: CodeGen e s ()
entryCode = do push ebp
               mov ebp esp

exitCode :: CodeGen e s ()
exitCode = do mov esp ebp
              pop ebp 
              ret



run :: [RegIL] -> Ptr Int32 -> IO (MateState, Either ErrMsg [H.Instruction])
run program env = let compileAndFeedback = mapM_ compileRegIL program >> disassemble
                  in runCodeGen compileAndFeedback env (MateState "none")


-- Allocates a buffer with size n. All zero.
emptyMemory ::  (Storable a, Num a) => Int -> IO (Ptr a)
emptyMemory n = mallocArray n 
                  >>= (\ptr -> pokeArray ptr (replicate n 0) >> return ptr)


testEnv p' = do 
              ptr <- emptyMemory 26
              (_, Right code) <- run p' ptr
              return $ map H.showIntel code


simpleTest ::  [RegIL]
simpleTest = [RMov 0 1]


-- Just some class file sand
loadMethod methodName classFile = do cls <- parseClassFile classFile
                                     dumpClass cls
                                     return (cls, lookupMethod methodName cls)


getFib = do (cls, Just m) <- loadMethod "fac" "../tests/Fac.class"
            return (cls, m)

fibBasicBlocks = do (cls,m) <- getFib
                    hmap <- parseMethod cls "facFor"
                    printMapBB hmap
                    return ()


fib = do con@(Just (ins,cls)) <- getMethodIO "../tests/Fac.class" "facFor"
         let offsets = getInstOffsets ins
         let taggedInst = zip3 (map Source $ sumSeries'' offsets) offsets ins
         mapM_ print taggedInst
         let continuations =  execState (findContinuations taggedInst) ([],H.empty)
         print continuations
         let cfg = buildCFGContext con
         print cfg
         return cfg 
 
fib' = do con@(Just (ins,cls)) <- getMethodIO "../tests/Fac.class" "facFor"
          let tagged = getInstructions ins
          print tagged
          let backRefs = splitBlocksBackRef tagged
          let splitted = splitBlocks backRefs tagged
          print splitted
          let transitions = getTransitions splitted
          let nodes       = getNodes splitted
          print "nodes:"
          print nodes
          print "transitions"
          print transitions
          let (Just finalCyclicStructure) = indirectCFGToG splitted
          print "Final result"
          print $ printG' finalCyclicStructure

main = do con@(Just (ins,cls)) <- getMethodIO "../tests/AbsurdlyHuge.class" "absurdlyHuge"
          let tagged = getInstructions ins
          let backRefs = splitBlocksBackRef tagged
          let splitted = splitBlocks backRefs tagged
          let transitions = getTransitions splitted
          let nodes       = getNodes splitted
          print "nodes:"
          print nodes
          print "transitions"
          print transitions
          let (Just finalCyclicStructure) = indirectCFGToG splitted
          print "Final result"
          print $ printG' finalCyclicStructure

    


diamant ::  G String
diamant = let start = Node "a" left right
              left  = Node "l" end Nil
              right = Node "r" end Nil
              end   = Node "g" start Nil
          in start

dag = Node "a" (Node "b" (Leaf "c") (Leaf "d")) (Node "b" (Leaf "c") (Leaf "d"))


value (Node val _ _) = Just val
value (Leaf val    ) = Just val
value Nil            = Nothing
                                          

printG' ::  Ord k => G k -> [k]
printG' g = evalState (foldGM ((: []), (++)) (\node -> let (Just v) = value node in v) [] g) S.empty
