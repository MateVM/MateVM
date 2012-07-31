{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Mate.Utilities where

import Data.Word
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Data.List

import JVM.ClassFile

import Mate.Types


buildMethodID :: Class Direct -> Word16 -> MethodInfo
buildMethodID cls idx = MethodInfo (ntName nt) rc (ntSignature nt)
  where
    (rc, nt) = case constsPool cls M.! idx of
      (CMethod rc' nt') -> (rc', nt')
      (CIfaceMethod rc' nt') -> (rc', nt')
      _ -> error "buildMethodID: something wrong. abort."

buildStaticFieldID :: Class Direct -> Word16 -> StaticFieldInfo
buildStaticFieldID cls idx = StaticFieldInfo rc (ntName fnt)
  where (CField rc fnt) = constsPool cls M.! idx

buildFieldOffset :: Class Direct -> Word16 -> (B.ByteString, B.ByteString)
buildFieldOffset cls idx = (rc, ntName fnt)
  where (CField rc fnt) = constsPool cls M.! idx

buildClassID :: Class Direct -> Word16 -> B.ByteString
buildClassID cls idx = cl
  where (CClass cl) = constsPool cls M.! idx

methodGetArgsCount :: Class Direct -> Word16 -> Word32
methodGetArgsCount cls idx = fromIntegral $ length args
  where
    nt = case constsPool cls M.! idx of
      (CMethod _ nt') -> nt'
      (CIfaceMethod _ nt') -> nt'
      _ -> error "methodGetArgsCount: something wrong. abort."
    (MethodSignature args _) = ntSignature nt

-- TODO(bernhard): Extend it to more than just int, and provide typeinformation
methodHaveReturnValue :: Class Direct -> Word16 -> Bool
methodHaveReturnValue cls idx = case ret of
    ReturnsVoid -> False;
    (Returns BoolType) -> True
    (Returns CharByte) -> True
    (Returns IntType) -> True;
    (Returns (Array _ _)) -> True
    (Returns (ObjectType _)) -> True;
    _ -> error $ "methodHaveReturnValue: todo: " ++ show ret
  where
    nt = case constsPool cls M.! idx of
      (CMethod _ nt') -> nt'
      (CIfaceMethod _ nt') -> nt'
      _ -> error "methodHaveReturnValue: something wrong. abort."
    (MethodSignature _ ret) = ntSignature nt

lookupMethodSig :: B.ByteString -> MethodSignature -> Class Direct -> Maybe (Method Direct)
lookupMethodSig name sig cls =
  find (\x -> methodName x == name && methodSignature x == sig) $ classMethods cls
