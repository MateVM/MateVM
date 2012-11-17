{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Compiler.Mate.Frontend.IR
 ( MateIR(..)
 , VirtualReg
 , HandlerMap
 , MaybeHandler
 , LiveAnnotation
 , liveAnnEmpty
 , CallingConv(..)
 , CallType(..)
 , OpType(..)
 , HVarX86(..)
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

type VirtualReg = Integer
type LiveAnnotation = S.Set VirtualReg {- vars which are live after this instruction -}

liveAnnEmpty :: LiveAnnotation
liveAnnEmpty = S.empty

data MateIR t e x where
  IRLabel :: LiveAnnotation
          -> Label
          -> HandlerMap -> MaybeHandler
          -> MateIR t C O

  IROp :: (Show t) => LiveAnnotation
                   -> OpType
                   -> t {- dst -}
                   -> t {- src1 -}
                   -> t {- src2 -}
                   -> MateIR t O O
  IRStore :: (Show t) => LiveAnnotation
                      -> RTPool t
                      -> t {- objectref -}
                      -> t {- src -}
                      -> MateIR t O O
  IRLoad :: (Show t) => LiveAnnotation
                     -> RTPool t
                     -> t {- objectref -}
                     -> t {- target -}
                     -> MateIR t O O
  IRMisc1 :: (Show t) => LiveAnnotation
                      -> Instruction
                      -> t {- one src -}
                      -> MateIR t O O
  IRMisc2 :: (Show t) => LiveAnnotation
                      -> Instruction
                      -> t {- dst -}
                      -> t {- src -}
                      -> MateIR t O O
  IRPrep :: (Show t) => CallingConv
                     -> S.Set t {- regs in use -}
                     -> MateIR t O O
  IRInvoke :: (Show t) => LiveAnnotation
                       -> RTPool t
                       -> Maybe t
                       -> CallType
                       -> MateIR t O O
  IRPush :: (Show t) => LiveAnnotation
                     -> Word8
                     -> t
                     -> MateIR t O O

  IRJump :: Label -> MateIR t O C
  IRIfElse :: (Show t) => LiveAnnotation
                       -> CMP
                       -> t -> t
                       -> Label -> Label
                       -> MateIR t O C
  IRExHandler :: [Label] -> MateIR t O C -- dummy instruction to reference exception handler
  IRSwitch :: (Show t) => LiveAnnotation
                       -> t {- src -}
                       -> [(Maybe Int32, Label)]
                       -> MateIR t O C
  IRReturn :: (Show t) => LiveAnnotation
                       -> Maybe t
                       -> MateIR t O C

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

data HVarX86
  = HIReg Reg32
  | HIConstant Int32
  | SpillIReg Disp
  | HFReg XMMReg
  | HFConstant Float
  | SpillFReg Disp
  deriving (Eq, Ord)

deriving instance Eq Disp
deriving instance Ord Disp

type PreGCPoint = [(HVarX86, VarType)]

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

data VarType = JInt | JFloat | JRef deriving (Show, Eq, Ord)

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
  entryLabel (IRLabel _ l _ _) = l
  successors (IRJump l) = [l]
  successors (IRIfElse _ _ _ _ l1 l2) = [l1, l2]
  successors (IRExHandler t) = t
  successors (IRSwitch _ _ t) = map snd t
  successors (IRReturn _ _) = []

{- show -}
instance Show (MateIR t e x) where
  show (IRLabel la l hmap handlerstart) = printf "label: %s:\n\texceptions: %s\n\thandlerstart? %s%s" (show l) (show hmap) (show handlerstart) (showAnno la)
  show (IROp la op vr v1 v2) = printf "\t%s %s,  %s, %s%s" (show op) (show vr) (show v1) (show v2) (showAnno la)
  show (IRLoad la rt obj dst) = printf "\t%s(%s) -> %s%s" (show obj) (show rt) (show dst) (showAnno la)
  show (IRStore la rt obj src) = printf "\t%s(%s) <- %s%s" (show obj) (show rt) (show src) (showAnno la)
  show (IRInvoke la x r typ) = printf "\tinvoke %s %s [%s]%s" (show x) (show r) (show typ) (showAnno la)
  show (IRPush la argnr x) = printf "\tpush(%d) %s%s" argnr (show x) (showAnno la)
  show (IRJump l) = printf "\tjump %s" (show l)
  show (IRIfElse la jcmp v1 v2 l1 l2) = printf "\tif (%s `%s` %s) then %s else %s%s" (show v1) (show jcmp) (show v2) (show l1) (show l2) (showAnno la)
  show (IRExHandler t) = printf "\texhandler: %s" (show t)
  show (IRSwitch la reg t) = printf "\tswitch(%s) -> %s%s" (show reg) (show t) (showAnno la)
  show (IRReturn la b) = printf "\treturn (%s)%s" (show b) (showAnno la)
  show (IRMisc1 la jins x) = printf "\tmisc1: \"%s\": %s%s" (show jins) (show x) (showAnno la)
  show (IRMisc2 la jins x y) = printf "\tmisc2: \"%s\": %s %s%s" (show jins) (show x) (show y) (showAnno la)
  show (IRPrep typ regs) = printf "\tcall preps (%s): %s" (show typ) (show regs)

instance Show HVarX86 where
  show (HIReg r32) = printf "%s" (show r32)
  show (HIConstant val) = printf "0x%08x" val
  show (SpillIReg (Disp d)) = printf "0x%02x(ebp[i])" d
  show (HFReg xmm) = printf "%s" (show xmm)
  show (HFConstant val) = printf "%2.2ff" val
  show (SpillFReg (Disp d)) = printf "0x%02x(ebp[f])" d

instance Show Var where
  show (VReg t n) = printf "%s(%02d)" (show t) n
  show (JIntValue n) = printf "0x%08x" n
  show (JFloatValue n) = printf "%2.2ff" n
  show JRefNull = printf "(null)"

showAnno :: LiveAnnotation -> String
showAnno live = printf "\n\t\tnow living:  %s" (show $ S.toList live)
{- /show -}
