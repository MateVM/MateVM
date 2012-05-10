{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#include "debug.h"
module Mate.ClassPool (
  getClassInfo,
  getClassFile,
  getMethodTable,
  getObjectSize,
  getMethodOffset,
  getFieldOffset,
  getStaticFieldAddr,
  getInterfaceMethodOffset
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
#ifdef DBG_CLASS
import JVM.Dump
#endif

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable

import JVM.ClassFile
import JVM.Converter

import Mate.BasicBlocks
import {-# SOURCE #-} Mate.MethodPool
import Mate.Types
import Mate.Utilities
import Mate.Debug
import Mate.GarbageAlloc

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
  -- (4+) one slot for "interface-table-ptr"
  return $ (+4) $ fromIntegral $ (ciMethodMap ci) M.! method

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

-- interface + method + signature plz!
getInterfaceMethodOffset :: B.ByteString -> B.ByteString -> B.ByteString -> IO (Word32)
getInterfaceMethodOffset ifname meth sig = do
  loadInterface ifname
  ifmmap <- get_interfacemethodmap >>= ptr2interfacemethodmap
  let k = ifname `B.append` meth `B.append` sig
  case M.lookup k ifmmap of
    Just w32 -> return $ (+4) w32
    Nothing -> error $ "getInterfaceMethodOffset: no offset set"


loadClass :: B.ByteString -> IO ClassInfo
loadClass path = do
  let rpath = toString $ path `B.append` ".class"
  cfile <- parseClassFile rpath
#ifdef DBG_CLASS
  dumpClass cfile
#endif
  -- load all interfaces, which are implemented by this class
  sequence_ [ loadInterface i | i <- interfaces cfile ]
  superclass <- case (path /= "java/lang/Object") of
      True -> do
        sc <- loadClass $ superClass cfile
        return $ Just $ sc
      False -> return $ Nothing

  (staticmap, fieldmap) <- calculateFields cfile superclass
  (methodmap, mbase) <- calculateMethodMap cfile superclass
  immap <- get_interfacemethodmap >>= ptr2interfacemethodmap

  -- allocate interface offset table for this class
  -- TODO(bernhard): we have some duplicates in immap (i.e. some
  --                 entries have the same offset), so we could
  --                 save some memory here.
  iftable <- mallocClassData ((4*) $ M.size immap)
  let w32_iftable = fromIntegral $ ptrToIntPtr iftable :: Word32
  -- store interface-table at offset 0 in method-table
  pokeElemOff (intPtrToPtr $ fromIntegral mbase) 0 w32_iftable
  printf_cp "staticmap: %s @ %s\n" (show staticmap) (toString path)
  printf_cp "fieldmap:  %s @ %s\n" (show fieldmap) (toString path)
  printf_cp "methodmap: %s @ %s\n" (show methodmap) (toString path)
  printf_cp "mbase: 0x%08x\n" mbase
  printf_cp "interfacemethod: %s @ %s\n" (show immap) (toString path)
  printf_cp "iftable: 0x%08x\n" w32_iftable
  virtual_map <- get_virtualmap >>= ptr2virtualmap
  let virtual_map' = M.insert mbase path virtual_map
  virtualmap2ptr virtual_map' >>= set_virtualmap

  class_map <- get_classmap >>= ptr2classmap
  let new_ci = ClassInfo path cfile staticmap fieldmap methodmap mbase False
  let class_map' = M.insert path new_ci class_map
  classmap2ptr class_map' >>= set_classmap
  return new_ci


loadInterface :: B.ByteString -> IO ()
loadInterface path = do
  imap <- get_interfacesmap >>= ptr2interfacesmap
  -- interface already loaded?
  case M.lookup path imap of
    Just _ -> return ()
    Nothing -> do
      printf_cp "interface: loading \"%s\"\n" $ toString path
      let ifpath = toString $ path `B.append` ".class"
      cfile <- parseClassFile ifpath
      -- load "superinterfaces" first
      sequence_ [ loadInterface i | i <- interfaces cfile ]
      immap <- get_interfacemethodmap >>= ptr2interfacemethodmap

      -- load map again, because there could be new entries now
      -- due to loading superinterfaces
      imap' <- get_interfacesmap >>= ptr2interfacesmap
      let max_off = fromIntegral $ (M.size immap) * 4
      -- create index of methods by this interface
      let mm = zipbase max_off (classMethods cfile)

      -- create for each method from *every* superinterface a entry to,
      -- but just put in the same offset as it is already in the map
      let (ifnames, methodnames) = unzip $ concat $
            [ zip (repeat ifname) (classMethods $ imap' M.! ifname)
            | ifname <- interfaces cfile ]
      let sm = zipWith (\x y -> (entry y, immap M.! (getname x y))) ifnames methodnames

      -- merge all offset tables
      let methodmap = (M.fromList sm) `M.union` (M.fromList mm) `M.union` immap
      interfacemethodmap2ptr methodmap >>= set_interfacemethodmap

      interfacesmap2ptr (M.insert path cfile imap') >>= set_interfacesmap
  where
  zipbase base = zipWith (\x y -> (entry y, x + base)) [0,4..]
  entry = getname path
  getname p y = p `B.append` (methodName y) `B.append` (encode $ methodSignature y)


calculateFields :: Class Resolved -> Maybe ClassInfo -> IO (FieldMap, FieldMap)
calculateFields cf superclass = do
    -- TODO(bernhard): correct sizes. int only atm

    let (sfields, ifields) = span (S.member ACC_STATIC . fieldAccessFlags) (classFields cf)

    staticbase <- mallocClassData ((fromIntegral $ length sfields) * 4)
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

    -- (+1): one slot for the interface-table-ptr
    methodbase <- mallocClassData (((+1) $ fromIntegral $ M.size methodmap) * 4)
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
          printf_cp "executing static initializer from %s now\n" (toString path)
          executeFuncPtr entry
          printf_cp "static initializer from %s done\n" (toString path)
        Nothing -> error $ "loadClass: static initializer not found (WTF?). abort"
    Nothing -> return ()

  class_map' <- get_classmap >>= ptr2classmap
  let new_ci = ci { ciInitDone = True }
  let class_map'' = M.insert path new_ci class_map'
  classmap2ptr class_map'' >>= set_classmap
  return new_ci
