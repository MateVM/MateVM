{-# LANGUAGE OverloadedStrings #-}
module Mate.Utilities where

import Data.Char
import Data.Word
import Data.Binary
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
-- import qualified Data.ByteString.Lazy.Char8 as B8
import Codec.Binary.UTF8.String hiding (encode,decode)

import JVM.ClassFile


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

buildMethodID :: Class Resolved -> Word16 -> B.ByteString
buildMethodID cls idx = (rc `B.append` dot) `B.append` (ntName nt) `B.append` nt'
  where
  (CMethod rc nt) = (constsPool cls) M.! idx
  nt' = encode $ ntSignature nt
  dot :: B.ByteString
  -- TODO(bernhard): WTF? why -XOverloadedStrings doesn't apply here?
  dot = B.pack $ map (fromIntegral . ord) "."
