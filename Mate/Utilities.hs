{-# LANGUAGE OverloadedStrings #-}
module Mate.Utilities where

import Data.Char
import Data.Word
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
-- import qualified Data.ByteString.Lazy.Char8 as B8
import Codec.Binary.UTF8.String hiding (encode,decode)

import JVM.ClassFile

import Debug.Trace


data MethodInfo = MethodInfo {
  methodname :: B.ByteString,
  classname :: B.ByteString,
  signature :: MethodSignature,
  index :: Word16 }

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


-- TODO: actually this function already exists in hs-java-0.3!
lookupMethod :: B.ByteString -> Class Resolved -> Maybe (Method Resolved)
lookupMethod name cls = look (classMethods cls)
  where
    look [] = Nothing
    look (f:fs)
      | methodName f == name = Just f
      | otherwise  = look fs

toString :: B.ByteString -> String
toString bstr = decodeString $ map (chr . fromIntegral) $ B.unpack bstr

buildMethodID :: Class Resolved -> Word16 -> MethodInfo
buildMethodID cls idx = MethodInfo (ntName nt) rc (ntSignature nt) idx
  where
  (CMethod rc nt) = (constsPool cls) M.! idx

methodGetArgsCount :: Class Resolved -> Word16 -> Word32
methodGetArgsCount cls idx = fromIntegral $ length args
  where
  (CMethod _ nt) = (constsPool cls) M.! idx
  (MethodSignature args _) = ntSignature nt

-- TODO(bernhard): Extend it to more than just int, and provide typeinformation
methodHaveReturnValue :: Class Resolved -> Word16 -> Bool
methodHaveReturnValue cls idx = case ret of
    ReturnsVoid -> False;
    (Returns IntType) -> True;
    _ -> error "methodHaveReturnValue: todo"
  where
  (CMethod _ nt) = (constsPool cls) M.! idx
  (MethodSignature _ ret) = ntSignature nt
