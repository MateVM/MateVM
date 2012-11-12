{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Compiler.Mate.Frontend.IR
 ( MateIR(..)
 , CallingConv(..)
 , CallType(..)
 , OpType(..)
 , HVar(..)
 , Var(..)
 , RTPool(..)
 , PreGCPoint
 , VarType(..)
 , varType
 ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Set as S
import Data.Word
import Data.Int
import Text.Printf

import JVM.Assembler
import Compiler.Hoopl
import Harpy hiding (Label)

import Compiler.Mate.Types

type HandlerMap = [(B.ByteString {- exception class -}
                   , Word32 {- handler entry -}
                   )]
type MaybeHandler = Maybe Word32
data MateIR t e x where
  IRLabel :: Label -> HandlerMap -> MaybeHandler -> MateIR t C O

  IROp :: (Show t) => OpType -> t -> t -> t -> MateIR t O O
  IRStore :: (Show t) => RTPool t
                      -> t {- objectref -}
                      -> t {- src -}
                      -> MateIR t O O
  IRLoad :: (Show t) => RTPool t
                     -> t {- objectref -}
                     -> t {- target -}
                     -> MateIR t O O
  IRMisc1 :: (Show t) => Instruction -> t -> MateIR t O O {- one src -}
  IRMisc2 :: (Show t) => Instruction -> t -> t -> MateIR t O O {- dst, src -}
  IRPrep :: (Show t) => CallingConv
                     -> S.Set t {- regs in use -}
                     -> MateIR t O O
  IRInvoke :: (Show t) => RTPool t
                       -> Maybe t
                       -> CallType
                       -> MateIR t O O
  IRPush :: (Show t) => Word8 -> t -> MateIR t O O

  IRJump :: Label -> MateIR t O C
  IRIfElse :: (Show t) => CMP -> t -> t -> Label -> Label -> MateIR t O C
  IRExHandler :: [Label] -> MateIR t O C -- dummy instruction to reference exception handler
  IRReturn :: (Show t) => Maybe t -> MateIR t O C

data CallingConv = SaveRegs | RestoreRegs deriving (Show, Eq)
data CallType = CallStatic | CallSpecial | CallVirtual | CallInterface deriving (Show, Eq)

data OpType
  = Add
  | Sub
  | Mul
  | Div
  | Rem
  | And
  | Or
  | Xor
  | ShiftLeft
  | ShiftRightArth
  | ShiftRightLogical
  deriving Show

data HVar
  = HIReg Reg32
  | HIConstant Int32
  | SpillIReg Disp
  | HFReg XMMReg
  | HFConstant Float
  | SpillFReg Disp
  | SpillRReg Disp
  deriving (Eq, Ord)

deriving instance Eq Disp
deriving instance Ord Disp

type PreGCPoint = [(HVar, VarType)]

data RTPool t
  = RTPool Word16
  | RTPoolCall Word16 PreGCPoint
  | RTArray Word8 MateObjType PreGCPoint Word32
  | RTIndex t VarType
  | RTNone

instance Show t => Show (RTPool t) where
  show (RTPool w16) = printf "RT(%02d)" w16
  show (RTPoolCall w16 _) = printf "RTCall(%02d)" w16
  show (RTIndex t typ) = printf "RTIdx(%s[%s])" (show t) (show typ)
  show RTNone = ""
  show (RTArray w8 mot _ len) =
    -- (concatMap (\x -> printf "\t\t%s\n" (show x)) regmap) ++
    (printf "Array(%02d, len=%s, %s)\n" w8 (show len) (show mot))

data VarType = JChar | JInt | JFloat | JRef deriving (Show, Eq, Ord)

data Var
  = JIntValue Int32
  | JFloatValue Float
  | VReg VarType Integer
  | JRefNull
  deriving (Eq, Ord)

varType :: Var -> VarType
varType (JIntValue _) = JInt
varType (JFloatValue _) = JFloat
varType (VReg t _) = t
varType JRefNull = JRef

instance NonLocal (MateIR Var) where
  entryLabel (IRLabel l _ _) = l
  successors (IRJump l) = [l]
  successors (IRIfElse _ _ _ l1 l2) = [l1, l2]
  successors (IRExHandler t) = t
  successors (IRReturn _) = []

{- show -}
instance Show (MateIR t e x) where
  show (IRLabel l hmap handlerstart) = printf "label: %s:\n\texceptions: %s\n\thandlerstart? %s\n" (show l) (show hmap) (show handlerstart)
  show (IROp op vr v1 v2) = printf "\t%s %s,  %s, %s" (show op) (show vr) (show v1) (show v2)
  show (IRLoad rt obj dst) = printf "\t%s(%s) -> %s" (show obj) (show rt) (show dst)
  show (IRStore rt obj src) = printf "\t%s(%s) <- %s" (show obj) (show rt) (show src)
  show (IRInvoke x r typ) = printf "\tinvoke %s %s [%s]" (show x) (show r) (show typ)
  show (IRPush argnr x) = printf "\tpush(%d) %s" argnr (show x)
  show (IRJump l) = printf "\tjump %s" (show l)
  show (IRIfElse jcmp v1 v2 l1 l2) = printf "\tif (%s `%s` %s) then %s else %s" (show v1) (show jcmp) (show v2) (show l1) (show l2)
  show (IRExHandler t) = printf "\texhandler: %s" (show t)
  show (IRReturn b) = printf "\treturn (%s)" (show b)
  show (IRMisc1 jins x) = printf "\tmisc1: \"%s\": %s" (show jins) (show x)
  show (IRMisc2 jins x y) = printf "\tmisc2: \"%s\": %s %s " (show jins) (show x) (show y)
  show (IRPrep typ regs) = printf "\tcall preps (%s): %s" (show typ) (show regs)

instance Show HVar where
  show (HIReg r32) = printf "%s" (show r32)
  show (HIConstant val) = printf "0x%08x" val
  show (SpillIReg (Disp d)) = printf "0x%02x(ebp[i])" d
  show (HFReg xmm) = printf "%s" (show xmm)
  show (HFConstant val) = printf "%2.2ff" val
  show (SpillFReg (Disp d)) = printf "0x%02x(ebp[f])" d
  show (SpillRReg (Disp d)) = printf "0x%02x(ebp[r])" d

instance Show Var where
  show (VReg t n) = printf "%s(%02d)" (show t) n
  show (JIntValue n) = printf "0x%08x" n
  show (JFloatValue n) = printf "%2.2ff" n
  show JRefNull = printf "(null)"
{- /show -}
