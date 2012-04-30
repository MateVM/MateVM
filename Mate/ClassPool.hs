{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Mate.ClassPool (
  getClassInfo,
  getClassFile,
  getMethodTable,
  getObjectSize,
  getMethodOffset,
  getFieldOffset,
  getStaticFieldAddr
  ) where

import Data.Int
import Data.Word
import Data.Binary
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B
import Control.Monad

#ifdef DEBUG
import Text.Printf
#endif

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
  return $ ciFile ci

getStaticFieldOffset :: B.ByteString -> B.ByteString -> IO (CUInt)
getStaticFieldOffset path field = do
  ci <- getClassInfo path
  return $ fromIntegral $ (ciStaticMap ci) M.! field

getFieldOffset :: B.ByteString -> B.ByteString -> IO (Int32)
getFieldOffset path field = do
  ci <- getClassInfo path
  return $ (ciFieldMap ci) M.! field

-- method + signature plz!
getMethodOffset :: B.ByteString -> B.ByteString -> IO (Word32)
getMethodOffset path method = do
  ci <- getClassInfo path
  return $ fromIntegral $ (ciMethodMap ci) M.! method

getMethodTable :: B.ByteString -> IO (Word32)
getMethodTable path = do
  ci <- getClassInfo path
  return $ ciMethodBase ci

getObjectSize :: B.ByteString -> IO (Word32)
getObjectSize path = do
  ci <- getClassInfo path
  -- TODO(bernhard): correct sizes for different types...
  let fsize = fromIntegral $ M.size $ ciFieldMap ci
  -- one slot for "method-table-ptr"
  return $ (1 + fsize) * 4

foreign export ccall getStaticFieldAddr :: CUInt -> Ptr () -> IO CUInt
getStaticFieldAddr :: CUInt -> Ptr () -> IO CUInt
getStaticFieldAddr from ptr_trapmap = do
  trapmap <- ptr2trapmap ptr_trapmap
  let w32_from = fromIntegral from
  let sfi = trapmap M.! w32_from
  case sfi of
    (SFI (StaticFieldInfo cls field)) -> do
      getStaticFieldOffset cls field
    _ -> error $ "getFieldAddr: no trapInfo. abort"

loadClass :: B.ByteString -> IO ClassInfo
loadClass path = do
#ifdef DEBUG
  printf "loadClass: \"%s\"\n" $ toString path
#endif
  let rpath = toString $ path `B.append` ".class"
  cfile <- parseClassFile rpath
  superclass <- case (path /= "java/lang/Object") of
      True -> do
        sc <- loadClass $ superClass cfile
        return $ Just $ sc
      False -> return $ Nothing

  (staticmap, fieldmap) <- calculateFields cfile superclass
  (methodmap, mbase) <- calculateMethodMap cfile superclass
#ifdef DEBUG
  printf "staticmap: %s @ %s\n" (show staticmap) (toString path)
  printf "fieldmap:  %s @ %s\n" (show fieldmap) (toString path)
  printf "methodmap: %s @ %s\n" (show methodmap) (toString path)
  printf "mbase: 0x%08x\n" mbase
#endif

  virtual_map <- get_virtualmap >>= ptr2virtualmap
  let virtual_map' = M.insert mbase path virtual_map
  virtualmap2ptr virtual_map' >>= set_virtualmap

  class_map <- get_classmap >>= ptr2classmap
  let new_ci = ClassInfo path cfile staticmap fieldmap methodmap mbase False
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
    let sc_sm = getsupermap superclass ciStaticMap
    -- new fields "overwrite" old ones, if they have the same name
    let staticmap = (M.fromList sm) `M.union` sc_sm

    let sc_im = getsupermap superclass ciFieldMap
    -- "+ 4" for the method table pointer
    let max_off = (fromIntegral $ (M.size sc_im) * 4) + 4
    let im = zipbase max_off ifields
    -- new fields "overwrite" old ones, if they have the same name
    let fieldmap = (M.fromList im) `M.union` sc_im

    return (staticmap, fieldmap)
  where
  zipbase base = zipWith (\x y -> (fieldName y, x + base)) [0,4..]

-- helper
getsupermap :: Maybe ClassInfo -> (ClassInfo -> FieldMap) -> FieldMap
getsupermap superclass getter = case superclass of Just x -> getter x; Nothing -> M.empty


calculateMethodMap :: Class Resolved -> Maybe ClassInfo -> IO (FieldMap, Word32)
calculateMethodMap cf superclass = do
    let methods = filter
                  (\x -> (not . S.member ACC_STATIC . methodAccessFlags) x &&
                         ((/=) "<init>" . methodName) x)
                  (classMethods cf)
    let sc_mm = getsupermap superclass ciMethodMap
    let max_off = fromIntegral $ (M.size sc_mm) * 4
    let mm = zipbase max_off methods
    let methodmap = (M.fromList mm) `M.union` sc_mm

    methodbase <- mallocBytes ((fromIntegral $ M.size methodmap) * 4)
    return (methodmap, fromIntegral $ ptrToIntPtr $ methodbase)
  where zipbase base = zipWith (\x y -> (entry y, x + base)) [0,4..]
          where entry y = (methodName y) `B.append` (encode $ methodSignature y)


loadAndInitClass :: B.ByteString -> IO ClassInfo
loadAndInitClass path = do
  class_map <- get_classmap >>= ptr2classmap
  ci <- case M.lookup path class_map of
    Nothing -> loadClass path
    Just x -> return x

  -- first try to execute class initializer of superclass
  when (path /= "java/lang/Object") ((loadAndInitClass $ superClass $ ciFile ci) >> return ())

  -- execute class initializer
  case lookupMethod "<clinit>" (ciFile ci) of
    Just m -> do
      hmap <- parseMethod (ciFile ci) "<clinit>"
      case hmap of
        Just hmap' -> do
          let mi = (MethodInfo "<clinit>" path (methodSignature m))
          entry <- compileBB hmap' mi
          addMethodRef entry mi [path]
#ifdef DEBUG
          printf "executing static initializer from %s now\n" (toString path)
#endif
          executeFuncPtr entry
#ifdef DEBUG
          printf "static initializer from %s done\n" (toString path)
#endif
        Nothing -> error $ "loadClass: static initializer not found (WTF?). abort"
    Nothing -> return ()

  class_map' <- get_classmap >>= ptr2classmap
  let new_ci = ci { ciInitDone = True }
  let class_map'' = M.insert path new_ci class_map'
  classmap2ptr class_map'' >>= set_classmap
  return new_ci
