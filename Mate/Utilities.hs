{-# LANGUAGE OverloadedStrings #-}
module Mate.Utilities where

import Data.Word
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B

import JVM.ClassFile

import Mate.Types


-- TODO: actually this function already exists in hs-java-0.3!
lookupMethod :: B.ByteString -> Class Resolved -> Maybe (Method Resolved)
lookupMethod name cls = look (classMethods cls)
  where
    look [] = Nothing
    look (f:fs)
      | methodName f == name = Just f
      | otherwise  = look fs

buildMethodID :: Class Resolved -> Word16 -> MethodInfo
buildMethodID cls idx = MethodInfo (ntName nt) rc (ntSignature nt)
  where
  (CMethod rc nt) = (constsPool cls) M.! idx

buildFieldID :: Class Resolved -> Word16 -> StaticFieldInfo
buildFieldID cls idx = StaticFieldInfo rc (ntName fnt)
  where (CField rc fnt) = (constsPool cls) M.! idx

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
