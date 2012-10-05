{-# LANGUAGE FlexibleInstances #-}
module Main where

import qualified Data.List as L
import Data.Int
import Data.Word
-- import Data.Foldable
import Control.Applicative
import Data.Monoid

import Control.Monad.State

import Debug.Trace
import Text.Printf

-- TODO: translation: JVMInstruction -> BasicBlock JVMInstruction


-- source IR (jvm bytecode)
data JVMInstruction
  = ICONST_0
  | ICONST_1
  | IPUSH Word32
  | ILOAD Word8  -- stoarge offset
  | ISTORE Word8 -- stoarge offset
  | IADD
  | IFEQ_ICMP Int16 -- signed relative offset
  | GOTO Int16
  | DUP
  | SWAP
  | INVOKE Word8 -- amount of arguments
  | RETURN
  deriving Show

{- generic basicblock datastructure -}
type BlockID = Int
data BasicBlock a = BasicBlock
  { bbID :: BlockID
  , code :: [a]
  , nextBlock :: (NextBlock a)
  }

data NextBlock a
  = Return
  | Jump (BlockRef a)
  | TwoJumps (BlockRef a) (BlockRef a)
  | Switch [BlockRef a]

data BlockRef a
  = Self
  | Ref (BasicBlock a)
{- /basicblock -}


{- pretty printing stuff. -}
instance Show a => Show (BasicBlock a) where
  show (BasicBlock bid insns end) = sbb ++ send
    where
      sbb = printf "BasicBlock%03d:\n" bid
            ++ concatMap (\x -> printf "\t%s\n" (show x)) insns
      send = show end

instance Show (NextBlock a) where
  show x = case x of
    Return -> ""
    Jump br -> printf "jump: %s\n\n" (show br)
    TwoJumps br1 br2 -> printf "jump1: %s, jump2: %s\n\n" (show br1) (show br2)
    Switch _ -> error "showNextBlock: switch"

instance Show (BlockRef a) where
  show Self = "self"
  show (Ref bb) = printf "BasicBlock%03d" (bbID bb)
{- /show -}



{- traverse the basiblock datastructure -}
type Visited = [BlockID]
type FoldState m = Monoid m => State Visited m

bbFold :: Monoid m => (BasicBlock a -> m) -> BasicBlock a -> m
bbFold f' bb' = evalState (bbFoldState f' bb') []
  where
    bbFoldState :: Monoid m => (BasicBlock a -> m) -> BasicBlock a -> FoldState m
    bbFoldState f bb@(BasicBlock bid _ next) = do
      visited <- get
      if bid `L.elem` visited
        then return mempty
        else do
          modify (bid:)
          let b = f bb
          case next of
            Return -> return b
            Jump ref -> do
              r1 <- brVisit f ref
              return $ b `mappend` r1
            TwoJumps ref1 ref2 -> do
              r1 <- brVisit f ref1
              r2 <- brVisit f ref2
              return $ b `mappend` r1 `mappend` r2
            Switch _ -> error "impl. switch stuff"
    brVisit :: (BasicBlock a -> m) -> BlockRef a -> FoldState m
    brVisit _ Self = return mempty
    brVisit f (Ref bb) = bbFoldState f bb
{- /traverse -}


ex0 :: BasicBlock JVMInstruction
ex0 = BasicBlock 1 [ICONST_0, ISTORE 0] $ Jump (Ref bb2)
  where
    bb2 = BasicBlock 2 [ILOAD 0, ICONST_1, IADD, ISTORE 0
                       , ILOAD 0, IPUSH 10, IFEQ_ICMP 0x1337]
                       $ TwoJumps Self (Ref bb3)
    bb3 = BasicBlock 3 [ILOAD 0, IPUSH 20, IFEQ_ICMP 0x1337]
                       $ TwoJumps (Ref bb2) (Ref bb4)
    bb4 = BasicBlock 4 [RETURN] Return

ex1 :: BasicBlock JVMInstruction
ex1 = BasicBlock 1 [RETURN] Return


-- TODO: this is a hack, is this defined somewhere?
instance Monoid (IO ()) where { mempty = pure mempty; mappend = (*>) }

main :: IO ()
main = do
  let extractbid = \(BasicBlock bid _ _) -> [bid]
  putStrLn $ "woot: " ++ (show $ GOTO 12)
  putStrLn $ "getlabels: ex0: " ++ (show $ bbFold extractbid ex0)
  putStrLn $ "getlabels: ex1: " ++ (show $ bbFold extractbid ex1)
  putStrLn "\n\n"
  bbFold print ex0
