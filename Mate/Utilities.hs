module Mate.Utilities where

import qualified Data.ByteString.Lazy as B

import JVM.ClassFile


-- TODO: actually this function already exists in hs-java-0.3!
lookupMethod :: B.ByteString -> Class Resolved -> Maybe (Method Resolved)
lookupMethod name cls = look (classMethods cls)
  where
    look [] = Nothing
    look (f:fs)
      | methodName f == name = Just f
      | otherwise  = look fs
