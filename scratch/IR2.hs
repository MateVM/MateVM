{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
module IR2 where

import Control.Monad.State
import Data.Map

data Ref

data TInt
data TFloat
data Target
data Source

data VReg s t u where
  IntReg :: Int -> VReg Int TInt u
  ObjRef :: Int -> VReg Int TInt u --pack obj references as IntRegs
  FloatReg :: Int -> VReg Int TFloat u


data IR s =  
  forall t . Add !(VReg s t Target) !(VReg s t Source) !(VReg s t Source)
 

createAdd :: VReg s t Target -> VReg s t Source -> VReg s t Source -> IR s
createAdd target op0 op1 = Add target op0 op1

add2Ints = createAdd (IntReg 0) (IntReg 1) (IntReg 2)
add2Floats = Add (FloatReg 0) (FloatReg 1) (FloatReg 3)

evalIRInt :: IR Int -> EvalM ()
evalIRInt (Add target op0 op1) = evalAdd target op0 op1

evalAdd :: VReg s t Target -> VReg s t Source -> VReg s t Source -> EvalM ()
evalAdd (IntReg x) (IntReg y) (IntReg z) = return ()

type EvalM a = State ExecutionState a
data ExecutionState = ExecutionState { floatRegs :: Map Int Float, intRegs :: Map Int Int }

eval :: EvalM a -> a
eval x = evalState x (ExecutionState { floatRegs = empty, intRegs = empty }) 

data AbstractJVMInstruction = 
   -- array instructions
     AaLoad  !Ref !Int  --aaload
   | AaStore !Ref !Int  --aastore
   | AArrayNew !Int
 
   -- references
   | ANull --aconst_null
   | ALoad !Int --Load a reference onto the stack from local variable n (aload*)
  
