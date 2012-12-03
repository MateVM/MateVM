{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Compiler.Mate.Frontend.IR
 ( MateIR(..)
 , VirtualReg(..)
 , VRegNR
 , PC
 , RegMapping
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
 , mapIR, defIR, useIR, varsIR, varsIR'
 ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Word
import Data.Int
import Text.Printf

import Control.Arrow

import JVM.Assembler
import Compiler.Hoopl
import Harpy hiding (Label, fst)

import Compiler.Mate.Types

type HandlerMap = [(B.ByteString {- exception class -}
                   , Word32 {- handler entry -}
                   )]
type MaybeHandler = Maybe Word32

type VRegNR = Integer

data VirtualReg = VR
  { vrNr :: VRegNR
  , vrTyp :: VarType
  } deriving Show

instance Eq VirtualReg where
  (VR x _) == (VR y _) = x == y

instance Ord VirtualReg where
  {-# INLINE compare #-}
  compare (VR x _) (VR y _) = x `compare` y

type PC = Int
type RegMapping = M.Map VirtualReg HVarX86

type LiveAnnotation = S.Set VirtualReg {- vars which are live after this instruction -}

liveAnnEmpty :: LiveAnnotation
liveAnnEmpty = S.empty

data MateIR t e x where
  IRLabel :: Label -> HandlerMap -> MaybeHandler -> MateIR t C O

  IROp :: (Show t) => OpType -> t {- dst -} -> t {- src1 -} -> t {- src2 -} -> MateIR t O O
  IRStore :: (Show t) => RTPool t -> t {- objectref -} -> t {- src -} -> MateIR t O O
  IRLoad  :: (Show t) => RTPool t -> t {- objectref -} -> t {- dst-} -> MateIR t O O
  IRMisc1 :: (Show t) => Instruction -> t {- src -} -> MateIR t O O
  IRMisc2 :: (Show t) => Instruction -> t {- dst -} -> t {- src -} -> MateIR t O O
  IRPrep  :: (Show t) => CallingConv -> [(t, VarType)] -> MateIR t O O
  IRInvoke :: (Show t) => RTPool t -> Maybe t -> CallType -> MateIR t O O
  IRPush  :: (Show t) => Word8 -> t -> MateIR t O O

  IRJump :: Label -> MateIR t O C
  IRIfElse :: (Show t) => CMP -> t -> t -> Label -> Label -> MateIR t O C
  IRExHandler :: [Label] -> MateIR t O C -- dummy instruction to reference exception handler
  IRSwitch :: (Show t) => t {- src -} -> [(Maybe Int32, Label)] -> MateIR t O C
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

type PreGCPoint t = [(t, VarType)]

data RTPool t
  = RTPool Word16
  | RTPoolCall Word16 (PreGCPoint t)
  | RTArray Word8 MateObjType (PreGCPoint t) t
  | RTIndex t VarType
  | RTNone

instance Show t => Show (RTPool t) where
  show (RTPool w16) = printf "RT(%02d)" w16
  show (RTPoolCall w16 _) = printf "RTCall(%02d)" w16
  show (RTIndex t typ) = printf "RTIdx(%s[%s])" (show t) (show typ)
  show RTNone = ""
  show (RTArray w8 mot _ len) =
    -- (concatMap (\x -> printf "\t\t%s\n" (show x)) regmap) ++
    printf "Array(%02d, len=%s, %s)\n" w8 (show len) (show mot)

data VarType = JInt | JFloat | JRef deriving (Show, Eq, Ord)

data Var
  = JIntValue Int32
  | JFloatValue Float
  | VReg VirtualReg
  | JRefNull
  deriving (Eq, Ord)

varType :: Var -> VarType
varType (JIntValue _) = JInt
varType (JFloatValue _) = JFloat
varType (VReg (VR _ t)) = t
varType JRefNull = JRef

instance NonLocal (MateIR Var) where
  entryLabel (IRLabel l _ _) = l
  successors (IRJump l) = [l]
  successors (IRIfElse _ _ _ l1 l2) = [l1, l2]
  successors (IRExHandler t) = t
  successors (IRSwitch _ t) = map snd t
  successors (IRReturn _) = []

{- show -}
instance Show (MateIR t e x) where
  show (IRLabel l hmap handlerstart) = printf "label: %s:\n\texceptions: %s\n\thandlerstart? %s" (show l) (show hmap) (show handlerstart)
  show (IROp op vr v1 v2) = printf "\t%s %s,  %s, %s" (show op) (show vr) (show v1) (show v2)
  show (IRLoad rt obj dst) = printf "\t%s(%s) -> %s" (show obj) (show rt) (show dst)
  show (IRStore rt obj src) = printf "\t%s(%s) <- %s" (show obj) (show rt) (show src)
  show (IRInvoke x r typ) = printf "\tinvoke %s %s [%s]" (show x) (show r) (show typ)
  show (IRPush argnr x) = printf "\tpush(%d) %s" argnr (show x)
  show (IRJump l) = printf "\tjump %s" (show l)
  show (IRIfElse jcmp v1 v2 l1 l2) = printf "\tif (%s `%s` %s) then %s else %s" (show v1) (show jcmp) (show v2) (show l1) (show l2)
  show (IRExHandler t) = printf "\texhandler: %s" (show t)
  show (IRSwitch reg t) = printf "\tswitch(%s) -> %s" (show reg) (show t)
  show (IRReturn b) = printf "\treturn (%s)" (show b)
  show (IRMisc1 jins x) = printf "\tmisc1: \"%s\": %s" (show jins) (show x)
  show (IRMisc2 jins x y) = printf "\tmisc2: \"%s\": %s %s" (show jins) (show x) (show y)
  show (IRPrep typ regs) = printf "\tcall preps (%s): %s" (show typ) (show regs)

instance Show HVarX86 where
  show (HIReg r32) = printf "%s" (show r32)
  show (HIConstant val) = printf "0x%08x" val
  show (SpillIReg (Disp d)) = printf "0x%02x(ebp[i])" d
  show (HFReg xmm) = printf "%s" (show xmm)
  show (HFConstant val) = printf "%2.2ff" val
  show (SpillFReg (Disp d)) = printf "0x%02x(ebp[f])" d

instance Show Var where
  show (VReg (VR n t)) = printf "%s(%02d)" (show t) n
  show (JIntValue n) = printf "0x%08x" n
  show (JFloatValue n) = printf "%2.2ff" n
  show JRefNull = printf "(null)"

showAnno :: LiveAnnotation -> String
showAnno _ = ""
-- showAnno live = printf "\n\t\tnow living:  %s" (show $ S.toList live)
{- /show -}

mapRT :: (t -> r) -> [(r, VarType)] -> RTPool t -> RTPool r
mapRT f _ (RTIndex var vt) = RTIndex (f var) vt
mapRT f mapping (RTArray w8 mobj _ var) = RTArray w8 mobj mapping (f var)
mapRT _ _ (RTPool w16) = RTPool w16
mapRT _ mapping (RTPoolCall w16 _) = RTPoolCall w16 mapping
mapRT _ _ RTNone = RTNone

varsRT' :: RTPool t -> ([t], [t])
varsRT' (RTIndex var _) = ([], [var])
varsRT' (RTArray _ _ _ var) = ([], [var])
varsRT' _ = ([], [])


mapIR :: Show r => (t -> r) -> [(r, VarType)] -> MateIR t e x -> MateIR r e x
mapIR _ _ (IRLabel l hmap mhand) = IRLabel l hmap mhand

mapIR f _ (IROp ot dst src1 src2) = IROp ot (f dst) (f src1) (f src2)
mapIR f mapping (IRStore rt oref src) = IRStore (mapRT f mapping rt) (f oref) (f src)
mapIR f mapping (IRLoad rt oref dst) = IRLoad (mapRT f mapping rt) (f oref) (f dst)
mapIR f _ (IRMisc1 ins src) = IRMisc1 ins (f src)
mapIR f _ (IRMisc2 ins src1 src2) = IRMisc2 ins (f src1) (f src2)
mapIR f _ (IRPrep ct emap) = IRPrep ct $ map (first f) emap
mapIR f mapping (IRInvoke rt Nothing ct) = IRInvoke (mapRT f mapping rt) Nothing ct
mapIR f mapping (IRInvoke rt (Just r) ct) = IRInvoke (mapRT f mapping rt) (Just (f r)) ct
mapIR f _ (IRPush w8 src) = IRPush w8 (f src)

mapIR _ _ (IRJump l) = IRJump l
mapIR f _ (IRIfElse jcmp src1 src2 l1 l2) = IRIfElse jcmp (f src1) (f src2) l1 l2
mapIR _ _ (IRExHandler lbls) = IRExHandler lbls
mapIR f _ (IRSwitch src smap) = IRSwitch (f src) smap
mapIR _ _ (IRReturn Nothing) = IRReturn Nothing
mapIR f _ (IRReturn (Just r)) = IRReturn (Just (f r))

defIR :: MateIR t e x -> [t]
defIR = fst . varsIR'

useIR :: MateIR t e x -> [t]
useIR = snd . varsIR'

varsIR :: MateIR t e x -> [t]
varsIR ins = defIR ins ++ useIR ins

varsIR' :: MateIR t e x -> ([t], [t])
varsIR' IRLabel{} = ([], [])

varsIR' (IROp _ dst src1 src2) = ([dst], [src1, src2])
varsIR' (IRStore rt oref src) = ([], [oref, src]) `tupcons` varsRT' rt
varsIR' (IRLoad rt oref dst) = ([dst], [oref]) `tupcons` varsRT' rt
varsIR' (IRMisc1 _ src) = ([], [src])
varsIR' (IRMisc2 _ src1 src2) = ([], [src1, src2])
varsIR' IRPrep{} = ([], [])
varsIR' (IRInvoke rt (Just r) _) = ([r], []) `tupcons` varsRT' rt
varsIR' IRInvoke{} = ([], [])
varsIR' (IRPush _ src) = ([], [src])

varsIR' (IRJump _) = ([], [])
varsIR' (IRIfElse _ src1 src2 _ _) = ([], [src1, src2])
varsIR' (IRExHandler _) = ([], [])
varsIR' (IRSwitch src _) = ([], [src])
varsIR' (IRReturn (Just r)) = ([], [r])
varsIR' (IRReturn _) = ([], [])

tupcons :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
tupcons (x1, y1) (x2, y2) = (x1 ++ x2, y1 ++ y2)
