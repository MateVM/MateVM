{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Mate.ClassPool where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B

import Text.Printf

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Alloc

import JVM.ClassFile
import JVM.Converter

import Mate.Types

getClassInfo :: B.ByteString -> IO ClassInfo
getClassInfo path = do
  ptr_classmap <- get_classmap
  class_map <- ptr2classmap ptr_classmap
  case M.lookup path class_map of
    Nothing -> loadClass path
    Just ci -> return ci

getClassFile :: B.ByteString -> IO (Class Resolved)
getClassFile path = do
  (ClassInfo _ cfile _ _) <- getClassInfo path
  return cfile

-- TODO(bernhard): I think we don't need that anymore. also remove fieldbase
--                 entry in ClassInfo
getFieldBase :: B.ByteString -> IO (CUInt)
getFieldBase path = do
  (ClassInfo _ _ fs _) <- getClassInfo path
  return $ fromIntegral $ ptrToIntPtr fs

getFieldOffset :: B.ByteString -> B.ByteString -> IO (CUInt)
getFieldOffset path field = do
  (ClassInfo _ _ _ fieldmap) <- getClassInfo path
  return $ fromIntegral $ fieldmap M.! field

foreign export ccall getFieldAddr :: CUInt -> Ptr () -> IO CUInt
getFieldAddr :: CUInt -> Ptr () -> IO CUInt
getFieldAddr from ptr_trapmap = do
  trapmap <- ptr2tmap ptr_trapmap
  let w32_from = fromIntegral from
  let sfi = trapmap M.! w32_from
  case sfi of
    (SFI (StaticFieldInfo cls field)) -> do
      getFieldOffset cls field
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
  class_map <- get_classmap >>= ptr2classmap
  -- TODO(bernhard): correct sizes. int only atm
  let filteredfields = filter (S.member ACC_STATIC . fieldAccessFlags) (classFields cfile)
  fieldbase <- mallocBytes ((fromIntegral $ length filteredfields) * 4)
  let i_fb = fromIntegral $ ptrToIntPtr $ fieldbase
  let fm = zipWith (\x y -> (fieldName y, x + i_fb)) [0,4..] filteredfields
  let sc_fm = case superclass of Just x -> clFieldMap x; Nothing -> M.empty
  -- new fields "overwrite" old ones, if they have the same name
  let fieldmap = (M.fromList fm) `M.union` sc_fm
  printf "fieldmap: %s @ %s\n" (show fieldmap) (toString path)
  let new_ci = ClassInfo path cfile fieldbase fieldmap
  let class_map' = M.insert path new_ci class_map
  classmap2ptr class_map' >>= set_classmap
  return new_ci
