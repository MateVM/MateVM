{-# LANGUAGE FlexibleInstances #-}
module Main where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Int
import Data.Word
-- import Data.Foldable
import Control.Applicative
import Data.Monoid

import Control.Monad.State

import Debug.Trace
import Text.Printf

-- TODO: translation: JVMInstruction -> BasicBlock JVMInstruction
-- TODO: rewrite with state info


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
bbFold f bb' = evalState (bbFoldState bb') []
  where
    -- TODO: type signature?!
    -- bbFoldState :: Monoid m => BasicBlock a -> FoldState m
    bbFoldState bb@(BasicBlock bid _ next) = do
      visited <- get
      if bid `L.elem` visited
        then return mempty
        else do
          modify (bid:)
          let b = f bb
          case next of
            Return -> return b
            Jump ref -> do
              r1 <- brVisit ref
              return $ b `mappend` r1
            TwoJumps ref1 ref2 -> do
              r1 <- brVisit ref1
              r2 <- brVisit ref2
              return $ b `mappend` r1 `mappend` r2
            Switch _ -> error "impl. switch stuff"
    -- brVisit :: BlockRef a -> FoldState m
    brVisit Self = return mempty
    brVisit (Ref bb) = bbFoldState bb
{- /traverse -}

{- rewrite basicblock: maintain structure, but transform instructions of
   basicblock -}
type BasicBlockMap a = M.Map BlockID (BasicBlock a)
type RewriteState a = State Visited (BasicBlockMap a)

-- TODO: refactor as state monad.  how?!
bbRewrite :: (BasicBlock a -> BasicBlock b) -> BasicBlock a -> BasicBlock b
bbRewrite f bb' = let (start, _) = bbRewrite' M.empty bb' in start
  where
    bbRewrite' visitmap bb@(BasicBlock bid _ next)
      | bid `M.member` visitmap = (visitmap M.! bid, visitmap)
      | otherwise = (x, newvmap)
          where
            x = (f bb) { nextBlock = newnext }
            visitmap' = M.insert bid x visitmap
            (newnext, newvmap) = case next of
              Return -> (Return, visitmap')
              Jump ref ->
                  let (r, m) = brVisit visitmap' ref
                  in (Jump r, m)
              TwoJumps ref1 ref2 ->
                  let (r1, m1) = brVisit visitmap' ref1
                      (r2, m2) = brVisit m1 ref2
                  in (TwoJumps r1 r2, m2)
              Switch _ -> error "impl. switch stuff (rewrite)"
    brVisit vmap Self = (Self, vmap)
    brVisit vmap (Ref bb) = (Ref r, m)
      where (r, m) = bbRewrite' vmap bb
{- /rewrite-}

bbRewriteWith :: (BasicBlock a -> State s (BasicBlock b)) -> s -> BasicBlock a -> BasicBlock b
bbRewriteWith f state' bb' = let (res, _, _) = bbRewrite' state' M.empty bb' in res
  where
    bbRewrite' state visitmap bb@(BasicBlock bid _ next)
      | bid `M.member` visitmap = (visitmap M.! bid, visitmap, state)
      | otherwise = (x, newvmap, allstate)
          where
            (x', newstate) = runState (f bb) state
            x = x' { nextBlock = newnext }
            visitmap' = M.insert bid x visitmap
            (newnext, newvmap, allstate) = case next of
              Return -> (Return, visitmap', newstate)
              Jump ref ->
                  let (r, m, s1) = brVisit newstate visitmap' ref
                  in (Jump r, m, s1)
              TwoJumps ref1 ref2 ->
                  let (r1, m1, s1) = brVisit newstate visitmap' ref1
                      (r2, m2, s2) = brVisit s1 m1 ref2
                  in (TwoJumps r1 r2, m2, s2)
              Switch _ -> error "impl. switch stuff (rewrite)"
    brVisit state vmap Self = (Self, vmap, state)
    brVisit state vmap (Ref bb) = (Ref r, m, newstate)
      where (r, m, newstate) = bbRewrite' state vmap bb


dummy = 0x1337 -- jumpoffset will be eliminated after basicblock analysis

ex0 :: BasicBlock JVMInstruction
ex0 = BasicBlock 1 [ICONST_0, ISTORE 0] $ Jump (Ref bb2)
  where
    bb2 = BasicBlock 2 [ILOAD 0, ICONST_1, IADD, ISTORE 0
                       , ILOAD 0, IPUSH 10, IFEQ_ICMP dummy]
                       $ TwoJumps Self (Ref bb3)
    bb3 = BasicBlock 3 [ILOAD 0, IPUSH 20, IFEQ_ICMP dummy]
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
  putStrLn "\n\n-- REWRITING (id) --\n\n"
  bbFold print $ bbRewrite id ex0
  putStrLn "\n\n-- REWRITING (dup code segment [indeed, it's pointless]) --\n\n"
  bbFold print $ bbRewrite (\bb -> bb { code = code bb ++ code bb }) ex0
  putStrLn "\n\n-- REWRITING WITH STATE --\n\n"
  let rewrite1 bb = do
        modify (+1)
        mul <- get
        return $ bb { code = concat $ take mul $ repeat (code bb) }
  bbFold print $ bbRewriteWith rewrite1 0 ex0
