{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Mate.Types where

import Data.Word
import Data.Int
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B

import Data.IORef
import System.IO.Unsafe

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

data RawMethod = RawMethod {
  rawMapBB :: MapBB,
  rawLocals :: Int,
  rawStackSize :: Int,
  rawArgCount :: Word32 }


-- Word32 = point of method call in generated code
-- MethodInfo = relevant information about callee
type TrapMap = M.Map Word32 TrapCause

data TrapCause =
  StaticMethod MethodInfo | -- for static calls
  VirtualMethod Bool MethodInfo | -- for virtual calls
  InterfaceMethod Bool MethodInfo | -- for interface calls
  StaticField StaticFieldInfo deriving Show

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

instance Show MethodInfo where
  show (MethodInfo method c sig) =
    toString c ++ "." ++ toString method ++ "." ++ show sig



-- store information of loaded classes
type ClassMap = M.Map B.ByteString ClassInfo

data ClassInfo = ClassInfo {
  ciName :: B.ByteString,
  ciFile :: Class Direct,
  ciStaticMap  :: FieldMap,
  ciFieldMap :: FieldMap,
  ciMethodMap :: FieldMap,
  ciMethodBase :: Word32,
  ciInitDone :: Bool }


-- store field offsets in a map
type FieldMap = M.Map B.ByteString Int32


-- java strings are allocated only once, therefore we
-- use a hashmap to store the address for a String
type StringMap = M.Map B.ByteString Word32


-- map "methodtable addr" to "classname"
-- we need that to identify the actual type
-- on the invokevirtual insn
type VirtualMap = M.Map Word32 B.ByteString


-- store each parsed Interface upon first loading
type InterfaceMap = M.Map B.ByteString (Class Direct)

-- store offset for each <Interface><Method><Signature> pair
type InterfaceMethodMap = M.Map B.ByteString Word32


{-
toString :: B.ByteString -> String
toString bstr = decodeString $ map (chr . fromIntegral) $ B.unpack bstr
-}

-- better solutions for a global map hack are welcome! (typeclasses, TH, ...?)

data MateCtx = MateCtx {
  ctxMethodMap :: MethodMap,
  ctxTrapMap :: TrapMap,
  ctxClassMap :: ClassMap,
  ctxVirtualMap :: VirtualMap,
  ctxStringMap :: StringMap,
  ctxInterfaceMap :: InterfaceMap,
  ctxInterfaceMethodMap :: InterfaceMethodMap }

emptyMateCtx :: MateCtx
emptyMateCtx = MateCtx M.empty M.empty M.empty M.empty M.empty M.empty M.empty

mateCtx :: IORef MateCtx
{-# NOINLINE mateCtx #-}
mateCtx = unsafePerformIO $ newIORef emptyMateCtx

-- TODO(bernhard): if we ever have thread support, don't forget MVars
#define SETMAP(name) set##name :: name -> IO (); \
  set##name m = do ctx <- readIORef mateCtx; \
  writeIORef mateCtx $ ctx { ctx##name = m };

#define GETMAP(name) get##name :: IO name ; \
  get##name = do ctx <- readIORef mateCtx; \
  return $ ctx##name ctx;

SETMAP(MethodMap);
GETMAP(MethodMap)

SETMAP(TrapMap)
GETMAP(TrapMap)

SETMAP(ClassMap)
GETMAP(ClassMap)

SETMAP(VirtualMap)
GETMAP(VirtualMap)

SETMAP(StringMap)
GETMAP(StringMap)

SETMAP(InterfaceMap)
GETMAP(InterfaceMap)

SETMAP(InterfaceMethodMap)
GETMAP(InterfaceMethodMap)
