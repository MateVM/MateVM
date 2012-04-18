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
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad.State

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


{- Thoughs on types and representations 
 - We start from constructing a CFG. What do we need here
 - ** Fast traversal which is aware of cycles
 - ** Fast successor, do we need predecessors?
 - ** Find all paths to current node (including back references)
 - ** Generic Node type in order to write mentioned operations
 -    generically. There should be no intermediate language "lock in"
 -    i.e. adding another IR should not kill CFG code
 -    Furthermore operations like SSA construction should
 -    not affect the CFG datastructure. Nodes contents should be 
 -    interchangable in a way.
 - ** Some form of unique naming - we would like to identify blocks
 -    and check whether code should be produced for this node
 - ** Should be Haskell idiomatic - should be composed with 
 -    standard haskell infrastructure
 - ** Convinient printing
 -
 - From this a inductive type should be appropriate?
 -
 -}

data G a = Node  a (G a) (G a)
         | Leaf  a 
         | Nil deriving(Show)

type LoopCheck a = Set a


{- Actually i am not sure about the cata and algebra definition.
 -
 - check: http://dl.acm.org/citation.cfm?id=128035
 -}

type TreeAlgebra a r = (a -> r, r -> r -> r)

foldG :: TreeAlgebra a r -> r -> G a -> r
foldG (f,g) s (Leaf val)     = g (f val) s
foldG (f,g) s (Node val l r) = g (f val) $ g (foldG (f,g) s l) (foldG (f,g) s r)
                        
printG = foldG ((: []), (++)) []

loopCheck :: (Ord k) => G a -> (G a -> k) -> State (LoopCheck k) Bool
loopCheck g f = do state <- get
                   return $ Set.member (f g) state

addNode :: (Ord k) => k -> State (LoopCheck k) ()
addNode k = do s <- get 
               put $ Set.insert k s
               return ()

foldGM :: (Ord k) => TreeAlgebra a r -> (G a -> k) -> r -> G a -> State (LoopCheck k) r
foldGM _     _ s    Nil           = return s
foldGM (f,g) c s k@(Leaf val)     = loopCheck k c >>= \p -> if p then return s else return $ g (f val) s
foldGM (f,g) c s k@(Node val l r) = loopCheck k c >>= \p -> if p then return s else continue
                                    where t        = foldGM (f,g) c s
                                          self     = g (f val)
                                          continue = do addNode $ c k
                                                        left  <- t l
                                                        right <- t r
                                                        return $ self $ g left right

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
printG' g = evalState (foldGM ((: []), (++)) (\node -> let (Just v) = value node in v) [] g) Set.empty

 


{- stupid sketch code -}

-- actually loop check does not work properly. use monadic version instead
foldG' :: (Ord k) => TreeAlgebra a r -> (G a -> k) -> r -> Set k -> G a -> r
foldG' (f,g) c s s' Nil              = s
foldG' (f,g) c s s' k@(Leaf val)     = if Set.member (c k) s' then s else g (f val) s
foldG' (f,g) c s s' k@(Node val l r) = if Set.member (c k) s' then s 
                                       else let newState = Set.insert (c k) s'
                                                left  = foldG' (f,g) c s newState l
                                                right = foldG' (f,g) c s newState r
                                             in g (f val) $ g left right


