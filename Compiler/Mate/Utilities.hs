{-# LANGUAGE OverloadedStrings #-}
module Compiler.Mate.Utilities where

import Data.Word
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Data.List

import JVM.ClassFile

import Compiler.Mate.Types
import Compiler.Mate.Backend.NativeSizes

buildMethodID :: Class Direct -> Word16 -> MethodInfo
buildMethodID cls idx = MethodInfo (ntName nt) rc (ntSignature nt)
  where
    (rc, nt) = case constsPool cls M.! idx of
      (CMethod rc' nt') -> (rc', nt')
      (CIfaceMethod rc' nt') -> (rc', nt')
      _ -> error "buildMethodID: something wrong. abort."

buildFieldOffset :: Class Direct -> Word16 -> (B.ByteString, B.ByteString)
buildFieldOffset cls idx = (rc, ntName fnt)
  where (CField rc fnt) = constsPool cls M.! idx

buildClassID :: Class Direct -> Word16 -> B.ByteString
buildClassID cls idx = cl
  where (CClass cl) = constsPool cls M.! idx


methodNameTypeByIdx :: Class Direct -> Word16 -> NameType (Method Direct)
methodNameTypeByIdx cls idx = case constsPool cls M.! idx of
  (CMethod _ nt') -> nt'
  (CIfaceMethod _ nt') -> nt'
  _ -> error "methodGetArgsCount: something wrong. abort."

methodGetArgsCount :: NameType (Method Direct) -> NativeWord
methodGetArgsCount nt = genericLength args
  where (MethodSignature args _) = ntSignature nt

lookupMethodWithSig :: B.ByteString -> MethodSignature -> Class Direct -> Maybe (Method Direct)
lookupMethodWithSig name sig cls =
  find (\x -> methodName x == name && methodSignature x == sig) $ classMethods cls

checkNothing :: String -> Maybe a -> a
checkNothing m Nothing   = error m
checkNothing _ (Just v)  = v
