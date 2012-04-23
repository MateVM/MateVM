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
      off <- getFieldOffset cls field
      base <- getFieldBase cls
      return $ base + off
    _ -> error $ "getFieldAddr: no trapInfo. abort"

loadClass :: B.ByteString -> IO ClassInfo
loadClass path = do
  ptr_classmap <- get_classmap
  class_map <- ptr2classmap ptr_classmap
  let rpath = toString $ path `B.append` ".class"
  cfile <- parseClassFile rpath
  printf "class fieldlength: %d\n" $ classFieldsCount cfile
  -- TODO(bernhard): correct sizes. int only atm
  let filteredfields = filter (S.member ACC_STATIC . fieldAccessFlags) (classFields cfile)
  let fm = zipWith (\x y -> (fieldName y, x)) [0,4..] filteredfields
  let fieldmap = M.fromList fm
  fieldbase <- mallocBytes ((fromIntegral $ M.size fieldmap) * 4)
  putStrLn $ "fieldmap: " ++ (show fieldmap)
  let new_ci = ClassInfo path cfile fieldbase fieldmap
  let class_map' = M.insert path new_ci class_map
  classmap2ptr class_map' >>= set_classmap
  return new_ci
