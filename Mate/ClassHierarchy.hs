module Mate.ClassHierarchy
  ( isInstanceOf
  ) where

import qualified Data.ByteString.Lazy as B
import Control.Applicative
import Text.Printf

import Mate.NativeSizes
import Mate.ClassPool


data Class
  = Class
    { clMtable :: NativeWord
    , clSuperClass :: Class
    , clInterfaces :: [Interface]
    }
  | JavaLangObject
    { clMtable :: NativeWord
    }

data Interface
  = Interface
    { ifSuperInterfaces :: [Interface]
    }

isInstanceOf :: NativeWord -> B.ByteString -> IO Bool
isInstanceOf obj_mtable classname = do
  (== obj_mtable) <$> getMethodTable classname
