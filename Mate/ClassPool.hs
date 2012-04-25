{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Mate.ClassPool (
  getClassInfo,
  getClassFile,
  getFieldAddr
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B
import Control.Monad

import Text.Printf

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Alloc

import JVM.ClassFile
import JVM.Converter

import Mate.BasicBlocks
import {-# SOURCE #-} Mate.MethodPool
import Mate.Types
import Mate.Utilities

getClassInfo :: B.ByteString -> IO ClassInfo
getClassInfo path = do
  class_map <- get_classmap >>= ptr2classmap
  case M.lookup path class_map of
    Nothing -> loadAndInitClass path
    Just ci -> return ci

getClassFile :: B.ByteString -> IO (Class Resolved)
getClassFile path = do
  ci <- getClassInfo path
  return $ clFile ci

getStaticFieldOffset :: B.ByteString -> B.ByteString -> IO (CUInt)
getStaticFieldOffset path field = do
  ci <- getClassInfo path
  return $ fromIntegral $ (clStaticMap ci) M.! field

getFieldOffset :: B.ByteString -> B.ByteString -> IO (CUInt)
getFieldOffset path field = do
  ci <- getClassInfo path
  return $ fromIntegral $ (clFieldMap ci) M.! field

foreign export ccall getFieldAddr :: CUInt -> Ptr () -> IO CUInt
getFieldAddr :: CUInt -> Ptr () -> IO CUInt
getFieldAddr from ptr_trapmap = do
  trapmap <- ptr2tmap ptr_trapmap
  let w32_from = fromIntegral from
  let sfi = trapmap M.! w32_from
  case sfi of
    (SFI (StaticFieldInfo cls field)) -> do
      getStaticFieldOffset cls field
    _ -> error $ "getFieldAddr: no trapInfo. abort"

loadClass :: B.ByteString -> IO ClassInfo
loadClass path = do
  printf "loadClass: \"%s\"\n" $ toString path
  let rpath = toString $ path `B.append` ".class"
  cfile <- parseClassFile rpath
  superclass <- case (path /= "java/lang/Object") of
      True -> do
        sc <- loadClass $ superClass cfile
        return $ Just $ sc
      False -> return $ Nothing

  (staticmap, fieldmap) <- calculateFields cfile superclass
  printf "staticmap: %s @ %s\n" (show staticmap) (toString path)
  printf "fieldmap:  %s @ %s\n" (show fieldmap) (toString path)

  class_map <- get_classmap >>= ptr2classmap
  let new_ci = ClassInfo path cfile staticmap fieldmap False
  let class_map' = M.insert path new_ci class_map
  classmap2ptr class_map' >>= set_classmap
  return new_ci


calculateFields :: Class Resolved -> Maybe ClassInfo -> IO (FieldMap, FieldMap)
calculateFields cf superclass = do
    -- TODO(bernhard): correct sizes. int only atm

    let (sfields, ifields) = span (S.member ACC_STATIC . fieldAccessFlags) (classFields cf)

    staticbase <- mallocBytes ((fromIntegral $ length sfields) * 4)
    let i_sb = fromIntegral $ ptrToIntPtr $ staticbase
    let sm = zipbase i_sb sfields
    let sc_sm = getsupermap clStaticMap
    -- new fields "overwrite" old ones, if they have the same name
    let staticmap = (M.fromList sm) `M.union` sc_sm

    let im = zipbase 0 ifields
    let sc_im = getsupermap clFieldMap
    -- new fields "overwrite" old ones, if they have the same name
    let fieldmap = (M.fromList im) `M.union` sc_im

    return (staticmap, fieldmap)
  where
  zipbase base = zipWith (\x y -> (fieldName y, x + base)) [0,4..]
  getsupermap getter = case superclass of Just x -> getter x; Nothing -> M.empty


loadAndInitClass :: B.ByteString -> IO ClassInfo
loadAndInitClass path = do
  class_map <- get_classmap >>= ptr2classmap
  ci <- case M.lookup path class_map of
    Nothing -> loadClass path
    Just x -> return x

  -- first try to execute class initializer of superclass
  when (path /= "java/lang/Object") ((loadAndInitClass $ superClass $ clFile ci) >> return ())

  -- execute class initializer
  case lookupMethod "<clinit>" (clFile ci) of
    Just m -> do
      hmap <- parseMethod (clFile ci) "<clinit>"
      printMapBB hmap
      case hmap of
        Just hmap' -> do
          let mi = (MethodInfo "<clinit>" path (methodSignature m))
          entry <- compileBB hmap' mi
          addMethodRef entry mi [path]
          printf "executing static initializer from %s now\n" (toString path)
          executeFuncPtr entry
          printf "static initializer from %s done\n" (toString path)
        Nothing -> error $ "loadClass: static initializer not found (WTF?). abort"
    Nothing -> return ()

  class_map' <- get_classmap >>= ptr2classmap
  let new_ci = ci { clInitDone = True }
  let class_map'' = M.insert path new_ci class_map'
  classmap2ptr class_map'' >>= set_classmap
  return new_ci
