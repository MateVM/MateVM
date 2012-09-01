{-# LANGUAGE GADTs #-}
module Analysis where

import Compiler.Hoopl
import Control.Monad.State

type Addr = Int

data StackIL = Dup | Ld Addr Type | Add Type | Store Addr Type deriving Show

type Target = (Int,Type)
type Source = (Int,Type)

tmpReg = 10

data RegIL = RMov Target Source | RAdd Target Source Source 
             | RStore Addr Source | RLoad Source Addr deriving Show

data Type = Int | Bottom deriving (Show,Eq)

type StackElem = (Int,Type)

type Stack = [StackElem]

dup :: State Stack ()
dup = modify (\(top@(i,t):xs) -> (i+1,t) : top : xs)

push :: Type -> State Stack Int
push t = do tos <- get
            case tos of 
              top@(i,_):xs -> put ((i + 1, t) : top : xs) >> return i
              [] -> put [(0,t)] >> return 0 

pop :: State Stack StackElem
pop = do x:xs <- get
         put xs
         return x

nextElem :: State Stack Int
nextElem = fmap ((+ 1) . fst . head) get

aInterpret' :: StackIL -> State Stack [RegIL]
aInterpret' Dup = dup >> return []
aInterpret' (Ld addr t) = do s <- push t
                             return [RLoad (s,t) addr]
aInterpret' (Store addr t) = do (xA,tA) <- pop 
                                return [RStore addr (xA,tA)]
aInterpret' (Add t) = do (iA,ta) <- pop
                         (iB,tb) <- pop 
                         push ta
                         if ta /= t || tb /=t then error "type mismatch in add"
                            else return [ RAdd (tmpReg,ta) (iA,ta) (iB,tb), 
                                          RMov (iB,tb) (tmpReg,ta)]

aInterpret :: [StackIL] -> State Stack [RegIL]
aInterpret = foldr (liftM2 (++) . aInterpret') (return [])

generateRegisterIR :: [StackIL] -> [RegIL]
generateRegisterIR = (`evalState` []) . aInterpret

--data StackIL = Dup | Ld Addr Type | Add Type | Store Addr Type
testCase1 =  [ Ld 0 Int, Ld 1 Int, Dup , Add Int, Add Int, Store 0 Int]

bench = concat . replicate 100000 

type TLabel = Int
type Var = String
data Expr = VarE Var | AddE Var Var | Lit Int | CLe Expr Expr

data Node e x where
  Label  :: TLabel -> Node C O
  Assign :: Var -> Expr -> Node O O
  If :: Expr -> TLabel -> TLabel -> Node O C
  Ret :: Node O C
  Nop :: Node O O
  Branch :: TLabel -> Node C C

{-testProgram = [ Assign "x" (Lit 3),
                Assign "y" (Lit 5),  
                If (CLe (VarE "x") (VarE "y")) 1 2,
                Assign "x" (VarE "y")
                ] -}


