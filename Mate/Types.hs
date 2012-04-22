module Mate.Types where

import Data.Char
import Data.Word
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Codec.Binary.UTF8.String hiding (encode,decode)

import JVM.ClassFile
import JVM.Assembler


type BlockID = Int
-- Represents a CFG node
data BasicBlock = BasicBlock {
                     code    :: [Instruction],
                     successor :: BBEnd }

-- describes (leaving) edges of a CFG node
data BBEnd = Return | FallThrough BlockID | OneTarget BlockID | TwoTarget BlockID BlockID deriving Show

type MapBB = M.Map BlockID BasicBlock


-- Word32 = point of method call in generated code
-- MethodInfo = relevant information about callee
type CMap = M.Map Word32 MethodInfo

-- B.ByteString = name of method
-- Word32 = entrypoint of method
type MMap = M.Map MethodInfo Word32



data MethodInfo = MethodInfo {
  methName :: B.ByteString,
  cName :: B.ByteString,
  mSignature :: MethodSignature,
  cpIndex :: Word16 }

instance Eq MethodInfo where
  (MethodInfo m_a c_a s_a i_a) == (MethodInfo m_b c_b s_b i_b) =
    (m_a == m_b) && (c_a == c_b) && (s_a == s_b) && (i_a == i_b)

-- TODO(bernhard): not really efficient. also, outsource that to hs-java
instance Ord MethodSignature where
  compare (MethodSignature args_a ret_a) (MethodSignature args_b ret_b)
    | cmp_args /= EQ = cmp_args
    | otherwise = (show ret_a) `compare` (show ret_b)
    where
    cmp_args = (show args_a) `compare` (show args_b)

instance Ord MethodInfo where
  compare (MethodInfo m_a c_a s_a i_a) (MethodInfo m_b c_b s_b i_b)
    | cmp_m /= EQ = cmp_m
    | cmp_c /= EQ = cmp_c
    | cmp_s /= EQ = cmp_s
    | otherwise = i_a `compare` i_b
    where
    cmp_m = m_a `compare` m_b
    cmp_c = c_a `compare` c_b
    cmp_s = s_a `compare` s_b

instance Show MethodInfo where
  show (MethodInfo method c sig idx) =
    (toString c) ++ "." ++ (toString method) ++ "." ++ (show sig) ++ "@" ++ (show idx)


toString :: B.ByteString -> String
toString bstr = decodeString $ map (chr . fromIntegral) $ B.unpack bstr
