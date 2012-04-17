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

import Harpy
import Harpy.X86Disassembler

import Foreign
import Control.Monad

import JVM.ClassFile
import JVM.Converter
import JVM.Dump

import qualified JVM.Assembler as JAsm

import Mate.Utilities
import Mate.BasicBlocks

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



run :: [RegIL] -> Ptr Int32 -> IO (MateState, Either ErrMsg [Instruction])
run program env = let compileAndFeedback = mapM_ compileRegIL program >> disassemble
                  in runCodeGen compileAndFeedback env (MateState "none")


-- Allocates a buffer with size n. All zero.
emptyMemory ::  (Storable a, Num a) => Int -> IO (Ptr a)
emptyMemory n = mallocArray n 
                  >>= (\ptr -> pokeArray ptr (replicate n 0) >> return ptr)


testEnv p' = do 
              ptr <- emptyMemory 26
              (_, Right code) <- run p' ptr
              return $ map showIntel code


simpleTest ::  [RegIL]
simpleTest = [RMov 0 1]


-- Just some class file sand
loadMethod methodName classFile = do cls <- parseClassFile classFile
                                     dumpClass cls
                                     return (cls, lookupMethod methodName cls)


getFib = do (cls, Just m) <- loadMethod "ackermann" "../tests/Ackermann.class"
            return (cls, m)

fibBasicBlocks = do (cls,m) <- getFib
                    hmap <- parseMethod cls "ackermann"
                    printMapBB hmap
                    return ()
