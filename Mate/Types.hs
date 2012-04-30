{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Mate.Types where

import Data.Char
import Data.Word
import Data.Int
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Codec.Binary.UTF8.String hiding (encode,decode)

import Foreign.Ptr
import Foreign.StablePtr

import JVM.ClassFile
import JVM.Assembler


type BlockID = Int
-- Represents a CFG node
data BasicBlock = BasicBlock {
  code :: [Instruction],
  successor :: BBEnd }

-- describes (leaving) edges of a CFG node
data BBEnd = Return | FallThrough BlockID | OneTarget BlockID | TwoTarget BlockID BlockID deriving Show

type MapBB = M.Map BlockID BasicBlock



-- Word32 = point of method call in generated code
-- MethodInfo = relevant information about callee
type TrapMap = M.Map Word32 TrapInfo

data TrapInfo =
  MI MethodInfo | -- for static calls
  VI MethodInfo | -- for virtual calls
  II MethodInfo | -- for interface calls
  SFI StaticFieldInfo deriving Show

data StaticFieldInfo = StaticFieldInfo {
  sfiClassName :: B.ByteString,
  sfiFieldName :: B.ByteString } deriving Show



-- B.ByteString = name of method
-- Word32 = entrypoint of method
type MethodMap = M.Map MethodInfo Word32

data MethodInfo = MethodInfo {
  methName :: B.ByteString,
  methClassName :: B.ByteString,
  methSignature :: MethodSignature
  } deriving (Eq, Ord)

-- TODO(bernhard): not really efficient. also, outsource that to hs-java
--                 deriving should be enough?
instance Ord MethodSignature where
  compare (MethodSignature args_a ret_a) (MethodSignature args_b ret_b)
    | cmp_args /= EQ = cmp_args
    | otherwise = (show ret_a) `compare` (show ret_b)
    where
    cmp_args = (show args_a) `compare` (show args_b)

instance Show MethodInfo where
  show (MethodInfo method c sig) =
    (toString c) ++ "." ++ (toString method) ++ "." ++ (show sig)



-- store information of loaded classes
type ClassMap = M.Map B.ByteString ClassInfo

data ClassInfo = ClassInfo {
  ciName :: B.ByteString,
  ciFile :: Class Resolved,
  ciStaticMap  :: FieldMap,
  ciFieldMap :: FieldMap,
  ciMethodMap :: FieldMap,
  ciMethodBase :: Word32,
  ciInitDone :: Bool }


-- store field offsets in a map
type FieldMap = M.Map B.ByteString Int32


-- java strings are allocated only once, therefore we
-- use a hashmap to store the address for a String
type StringsMap = M.Map B.ByteString Word32


-- map "methodtable addr" to "classname"
-- we need that to identify the actual type
-- on the invokevirtual insn
type VirtualMap = M.Map Word32 B.ByteString


-- store each parsed Interface upon first loading
type InterfacesMap = M.Map B.ByteString (Class Resolved)

-- store offset for each <Interface><Method><Signature> pair
type InterfaceMethodMap = M.Map B.ByteString Word32


toString :: B.ByteString -> String
toString bstr = decodeString $ map (chr . fromIntegral) $ B.unpack bstr


-- those functions are for the "global map hax"
-- TODO(bernhard): other solution please
foreign import ccall "get_trapmap"
  get_trapmap :: IO (Ptr ())

foreign import ccall "set_trapmap"
  set_trapmap :: Ptr () -> IO ()

foreign import ccall "get_methodmap"
  get_methodmap :: IO (Ptr ())

foreign import ccall "set_methodmap"
  set_methodmap :: Ptr () -> IO ()

foreign import ccall "get_classmap"
  get_classmap :: IO (Ptr ())

foreign import ccall "set_classmap"
  set_classmap :: Ptr () -> IO ()

foreign import ccall "get_virtualmap"
  get_virtualmap :: IO (Ptr ())

foreign import ccall "set_virtualmap"
  set_virtualmap :: Ptr () -> IO ()

foreign import ccall "get_stringsmap"
  get_stringsmap :: IO (Ptr ())

foreign import ccall "set_stringsmap"
  set_stringsmap :: Ptr () -> IO ()

foreign import ccall "get_interfacesmap"
  get_interfacesmap :: IO (Ptr ())

foreign import ccall "set_interfacesmap"
  set_interfacesmap :: Ptr () -> IO ()

foreign import ccall "get_interfacemethodmap"
  get_interfacemethodmap :: IO (Ptr ())

foreign import ccall "set_interfacemethodmap"
  set_interfacemethodmap :: Ptr () -> IO ()

-- TODO(bernhard): make some typeclass magic 'n stuff
--                 or remove that sh**
methodmap2ptr :: MethodMap -> IO (Ptr ())
methodmap2ptr methodmap = do
  ptr_methodmap <- newStablePtr methodmap
  return $ castStablePtrToPtr ptr_methodmap

ptr2methodmap :: Ptr () -> IO MethodMap
ptr2methodmap methodmap = deRefStablePtr $ ((castPtrToStablePtr methodmap) :: StablePtr MethodMap)

trapmap2ptr :: TrapMap -> IO (Ptr ())
trapmap2ptr trapmap = do
  ptr_trapmap <- newStablePtr trapmap
  return $ castStablePtrToPtr ptr_trapmap

ptr2trapmap :: Ptr () -> IO TrapMap
ptr2trapmap vmap = deRefStablePtr $ ((castPtrToStablePtr vmap) :: StablePtr trapmap)

classmap2ptr :: ClassMap -> IO (Ptr ())
classmap2ptr cmap = do
  ptr_cmap <- newStablePtr cmap
  return $ castStablePtrToPtr ptr_cmap

ptr2classmap :: Ptr () -> IO ClassMap
ptr2classmap vmap = deRefStablePtr $ ((castPtrToStablePtr vmap) :: StablePtr cmap)

virtualmap2ptr :: VirtualMap -> IO (Ptr ())
virtualmap2ptr cmap = do
  ptr_cmap <- newStablePtr cmap
  return $ castStablePtrToPtr ptr_cmap

ptr2virtualmap :: Ptr () -> IO VirtualMap
ptr2virtualmap vmap = deRefStablePtr $ ((castPtrToStablePtr vmap) :: StablePtr cmap)


stringsmap2ptr :: StringsMap -> IO (Ptr ())
stringsmap2ptr cmap = do
  ptr_cmap <- newStablePtr cmap
  return $ castStablePtrToPtr ptr_cmap

ptr2stringsmap :: Ptr () -> IO StringsMap
ptr2stringsmap vmap = deRefStablePtr $ ((castPtrToStablePtr vmap) :: StablePtr cmap)


interfacesmap2ptr :: InterfacesMap -> IO (Ptr ())
interfacesmap2ptr cmap = do
  ptr_cmap <- newStablePtr cmap
  return $ castStablePtrToPtr ptr_cmap

ptr2interfacesmap :: Ptr () -> IO InterfacesMap
ptr2interfacesmap vmap = deRefStablePtr $ ((castPtrToStablePtr vmap) :: StablePtr cmap)


interfacemethodmap2ptr :: InterfaceMethodMap -> IO (Ptr ())
interfacemethodmap2ptr cmap = do
  ptr_cmap <- newStablePtr cmap
  return $ castStablePtrToPtr ptr_cmap

ptr2interfacemethodmap :: Ptr () -> IO InterfaceMethodMap
ptr2interfacemethodmap vmap = deRefStablePtr $ ((castPtrToStablePtr vmap) :: StablePtr cmap)
