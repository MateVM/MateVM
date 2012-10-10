{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Int
import Data.Word
import Data.Typeable
import Control.Applicative
import Data.Monoid

import Compiler.Hoopl

import Control.Monad.State

-- import Debug.Trace
import Text.Printf


-- TODO: translation: JVMInstruction -> BasicBlock JVMInstruction
-- TODO: bbFold with state


-- source IR (jvm bytecode)
data JVMInstruction
  = ICONST_0
  | ICONST_1
  | FCONST_0
  | FCONST_1
  | IPUSH Int32
  | ILOAD Word8  -- storage offset
  | FLOAD Word8  -- storage offset
  | ISTORE Word8 -- storage offset
  | FSTORE Word8 -- storage offset
  | IADD
  | ISUB
  | IMUL
  | FADD
  | IFEQ_ICMP Int16 -- signed relative offset
  | GOTO Int16
  | DUP
  | SWAP
  | INVOKE Word8 -- amount of arguments
  | RETURN
  deriving Show

-- type Label = String

-- java types
data JInt = JInt Int32 deriving Typeable
data JFloat = JFloat Float deriving Typeable

data GeneralIR where
  IRLabel :: Label -> GeneralIR
  IRMov :: Var t -> Var t -> GeneralIR {- dest, src -}
  IROp :: OpType -> Var t -> Var t -> Var t -> GeneralIR {- dest, src1, src2 -}
  IRJump :: GeneralIR
  IRIfElse :: Var JInt -> Var JInt -> GeneralIR
  IRReturn :: Bool -> GeneralIR
  IRInvoke :: Word8 -> GeneralIR -- just a single returnvalue
  IRNop :: GeneralIR

data OpType
  = Add
  | Sub
  | Mul
  deriving Show

class Typeable t => Variable t where
  reg :: Int -> Var t
  constant :: t -> Var t -- TODO: dafuq

instance Variable JInt where
  reg = IReg
  constant = IConstant

instance Variable JFloat where
  reg = FReg
  constant = FConstant

data Var t where
  IReg :: Int -> Var JInt
  IConstant :: JInt -> Var JInt
  FReg :: Int -> Var JFloat
  FConstant :: JFloat -> Var JFloat
  -- TODO: mem stuff?
  deriving Typeable


{- generic basicblock datastructure -}
type BlockID = Int
data BasicBlock a = BasicBlock
  { bbID :: BlockID
  , code :: a
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
  show (BasicBlock bid insns end) =
       printf "BasicBlock%03d:\n" bid ++ show insns ++ show end

instance Show (NextBlock a) where
  show x = case x of
    Return -> ""
    Jump br -> printf "jump: %s\n\n" (show br)
    TwoJumps br1 br2 -> printf "jump1: %s, jump2: %s\n\n" (show br1) (show br2)
    Switch _ -> error "showNextBlock: switch"

instance Show (BlockRef a) where
  show Self = "self"
  show (Ref bb) = printf "BasicBlock%03d" (bbID bb)

instance Show [JVMInstruction] where
  show insns = concatMap (\x -> printf "\t%s\n" (show x)) insns

instance Show (GeneralIR) where
  show (IRLabel l) = printf "label \"%s\"" (show l)
  show (IRMov v1 v2) = printf "\tmov %s, %s\n" (show v1) (show v2)
  show (IROp op vr v1 v2) = printf "\t%s %s,  %s, %s\n" (show op) (show vr) (show v1) (show v2)
  show (IRInvoke x) = printf "\tinvoke %s\n" (show x)
  show IRJump = printf "\tjump\n"
  show (IRIfElse v1 v2) = printf "\tif (%s == %s)\n" (show v1) (show v2)
  show (IRReturn b) = printf "\treturn (%s)\n" (show b)
  show IRNop = printf "\tnop\n"

instance Show (Var t) where
  show (IReg n) = printf "i(%02d)" n
  show (IConstant (JInt val)) = printf "0x%08x" val
  show (FReg n) = printf "f(%02d)" n
  show (FConstant (JFloat val)) = printf "%2.2ff" val
{- /show -}



{- traverse the basicblock datastructure -}
type Visited = [BlockID]
type FoldState m = Monoid m => State Visited m

-- TODO: this is a hack, is this defined somewhere?
instance Monoid (IO ()) where { mempty = return mempty; mappend = (>>) }

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

bbRewrite :: (BasicBlock a -> BasicBlock b) -> BasicBlock a -> BasicBlock b
bbRewrite f bb' = bbRewriteWith f' () bb'
  where f' x _ = return $ f x
{- /rewrite-}

{- rewrite with state -}
type Transformer a b s = (BasicBlock a -> NextBlock b -> State s (BasicBlock b))

-- TODO: refactor as state monad.  how?!
--      "tying the knot" not possible with state monad?
bbRewriteWith :: Transformer a b s -> s -> BasicBlock a -> BasicBlock b
bbRewriteWith f state' bb' = let (res, _, _) = bbRewrite' state' M.empty bb' in res
  where
    bbRewrite' st visitmap bb@(BasicBlock bid _ next)
      | bid `M.member` visitmap = (visitmap M.! bid, visitmap, st)
      | otherwise = (x, newvmap, allstate)
          where
            (x, newstate) = runState (f bb newnext) st
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
    brVisit st vmap Self = (Self, vmap, st)
    brVisit st vmap (Ref bb) = (Ref r, m, newstate)
      where (r, m, newstate) = bbRewrite' st vmap bb
{- /rewrite with -}

{- JVMInstruction -> GeneralIR -}
data SimStack = SimStack
  { stack :: [StackElem]
  , iregcnt :: Int
  , fregcnt :: Int }

data StackElem where
  StackElem :: (Variable t, Typeable t) => Var t -> StackElem

transformJ2IR :: BasicBlock [JVMInstruction]
                 -> NextBlock [GeneralIR]
                 -> State SimStack (BasicBlock [GeneralIR])
transformJ2IR jvmbb next = do
  res <- filter noNop <$> mapM tir (code jvmbb)
  return (BasicBlock (bbID jvmbb) res next)
  where
    noNop IRNop = False; noNop _ = True

    tir :: JVMInstruction -> State SimStack GeneralIR
    tir ICONST_0 = tir (IPUSH 0)
    tir ICONST_1 = tir (IPUSH 1)
    tir (IPUSH x) = do push (IConstant $ JInt x); return IRNop
    tir FCONST_0 = do push (FConstant $ JFloat 0); return IRNop
    tir FCONST_1 = do push (FConstant $ JFloat 1); return IRNop
    tir (ILOAD x) = do push $ IReg (fromIntegral x); return IRNop
    tir (ISTORE y) = do
      x <- pop
      return $ IRMov (IReg $ fromIntegral y) x
    tir (FSTORE y) = do
      x <- pop
      return $ IRMov (FReg $ fromIntegral y) x
    tir IADD = tirOpInt Add
    tir ISUB = tirOpInt Sub
    tir IMUL = tirOpInt Mul
    tir FADD = do
      x <- pop; y <- pop; newvar <- newfvar
      push newvar
      return $ IROp Add newvar x y
    tir (IFEQ_ICMP _) = do
      x <- pop; y <- pop
      return $ IRIfElse x y
    tir RETURN = return $ IRReturn False
    tir x = error $ "tir: " ++ show x

    tirOpInt op = do
      x <- pop; y <- pop
      newvar <- newivar; push newvar
      return $ IROp op newvar x y

    -- helper
    newivar = do
      sims <- get
      put $ sims { iregcnt = (iregcnt sims) + 1 }
      return $ IReg $ iregcnt sims
    newfvar = do
      sims <- get
      put $ sims { fregcnt = (fregcnt sims) + 1 }
      return $ FReg $ fregcnt sims
    push x = do
      sims <- get
      put $ sims { stack = ((StackElem x):stack sims) }
    pop :: Typeable t => State SimStack (Var t)
    pop = do
      sims <- get
      put $ sims { stack = tail $ stack sims }
      return $ case head $ stack sims of
                StackElem x -> case cast x of
                        Just x' -> x'
                        Nothing -> error "abstract intrp.: invalid bytecode?"
{- /JVMInstruction -> GeneralIR -}


{- application -}
dummy = 0x1337 -- jumpoffset will be eliminated after basicblock analysis

ex0 :: BasicBlock [JVMInstruction]
ex0 = BasicBlock 1 [ICONST_0, ISTORE 0] $ Jump (Ref bb2)
  where
    bb2 = BasicBlock 2 [ILOAD 0, ICONST_1, IADD, ISTORE 0
                       , ILOAD 0, IPUSH 10, IFEQ_ICMP dummy]
                       $ TwoJumps Self (Ref bb3)
    bb3 = BasicBlock 3 [ILOAD 0, IPUSH 20, IFEQ_ICMP dummy]
                       $ TwoJumps (Ref bb2) (Ref bb4)
    bb4 = BasicBlock 4 [FCONST_0, FCONST_1, FADD, FSTORE 1, IPUSH 20
                       , IPUSH 1, IMUL, ISTORE 2, RETURN] Return

ex1 :: BasicBlock [JVMInstruction]
ex1 = BasicBlock 1 [RETURN] Return


main :: IO ()
main = do
  putStrLn "\n-- PRINT ex0 --\n\n"
  bbFold print ex0
  putStrLn "\n-- PRINT ex0 as GeneralIR --\n\n"
  bbFold print $ bbRewriteWith transformJ2IR (SimStack [] 50000 60000) ex0

oldmain :: IO ()
oldmain = do
  let extractbid = \(BasicBlock bid _ _) -> [bid]
  putStrLn $ "woot: " ++ (show $ GOTO 12)
  putStrLn $ "getlabels: ex0: " ++ (show $ bbFold extractbid ex0)
  putStrLn $ "getlabels: ex1: " ++ (show $ bbFold extractbid ex1)
  putStrLn "\n\n-- REWRITING (id) --\n\n"
  bbFold print $ bbRewrite id ex0
  putStrLn "\n\n-- REWRITING (dup code segment [indeed, it's pointless]) --\n\n"
  bbFold print $ bbRewrite (\bb -> bb { code = code bb ++ code bb }) ex0
  putStrLn "\n\n-- REWRITING WITH STATE --\n\n"
  let rewrite1 bb _ = do
        modify (+1)
        mul <- get
        return $ bb { code = concat $ take mul $ repeat (code bb) }
  bbFold print $ bbRewriteWith rewrite1 0 ex0
{- /application -}
