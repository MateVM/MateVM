module Mate.ClassHierarchy
  ( isInstanceOf
  , addClassEntry
  ) where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Control.Applicative
import Control.Monad
import Text.Printf

import Foreign
import Data.IORef

import Mate.NativeSizes
import Mate.ClassPool


data Class
  = Class
    { clMtable :: NativeWord
    , clSuperClass :: NativeWord
    , clInterfaces :: [Interface]
    }
  | JavaLangObject
    { clMtable :: NativeWord
    }

data Interface
  = Interface
    { ifSuperInterfaces :: [Interface]
    }

type HierMap = (M.Map NativeWord Class)
classHier :: IORef HierMap
{-# NOINLINE classHier #-}
classHier = unsafePerformIO $ newIORef M.empty

readHier :: IO HierMap
readHier = readIORef classHier

writeHier :: HierMap -> IO ()
writeHier = writeIORef classHier


isInstanceOf :: NativeWord -> B.ByteString -> IO Bool
isInstanceOf 0 _ = return False
isInstanceOf obj classname = do
  obj_mtable <- peek (intPtrToPtr . fromIntegral $ obj)
  class_mtable <- getMethodTable classname
  ch <- readHier
  return $ checkInstance obj_mtable class_mtable ch

checkInstance :: NativeWord -> NativeWord -> HierMap -> Bool
checkInstance obj cl_mtable ch
  | obj == cl_mtable = True
  | otherwise =
      case ch M.! obj of
        Class _ super _ -> checkInstance super cl_mtable ch
        JavaLangObject _ -> False

addClassEntry :: NativeWord -> NativeWord -> IO ()
addClassEntry mtable 0 = do
  ch <- readHier
  writeHier (M.insert mtable (JavaLangObject mtable) ch)
addClassEntry mtable super_mtable = do
  ch <- readHier
  when (not $ M.member super_mtable ch) $ error "classhierarchy: superclass should be in hierarchy!"
  let cl = Class mtable super_mtable []
  writeHier (M.insert mtable cl ch)
