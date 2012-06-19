{-# LANGUAGE OverloadedStrings #-}
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


setMethodMap :: MethodMap -> IO ()
setMethodMap m = do
  ctx <- readIORef mateCtx
  writeIORef mateCtx $ ctx { ctxMethodMap = m }

getMethodMap :: IO MethodMap
getMethodMap = do
  ctx <- readIORef mateCtx
  return $ ctxMethodMap ctx


setTrapMap :: TrapMap -> IO ()
setTrapMap m = do
  ctx <- readIORef mateCtx
  writeIORef mateCtx $ ctx { ctxTrapMap = m }

getTrapMap :: IO TrapMap
getTrapMap = do
  ctx <- readIORef mateCtx
  return $ ctxTrapMap ctx


setClassMap :: ClassMap -> IO ()
setClassMap m = do
  ctx <- readIORef mateCtx
  writeIORef mateCtx $ ctx { ctxClassMap = m }

getClassMap :: IO ClassMap
getClassMap = do
  ctx <- readIORef mateCtx
  return $ ctxClassMap ctx


setVirtualMap :: VirtualMap -> IO ()
setVirtualMap m = do
  ctx <- readIORef mateCtx
  writeIORef mateCtx $ ctx { ctxVirtualMap = m }

getVirtualMap :: IO VirtualMap
getVirtualMap = do
  ctx <- readIORef mateCtx
  return $ ctxVirtualMap ctx


setStringMap :: StringMap -> IO ()
setStringMap m = do
  ctx <- readIORef mateCtx
  writeIORef mateCtx $ ctx { ctxStringMap = m }

getStringMap :: IO StringMap
getStringMap = do
  ctx <- readIORef mateCtx
  return $ ctxStringMap ctx


setInterfaceMap :: InterfaceMap -> IO ()
setInterfaceMap m = do
  ctx <- readIORef mateCtx
  writeIORef mateCtx $ ctx { ctxInterfaceMap = m }

getInterfaceMap :: IO InterfaceMap
getInterfaceMap = do
  ctx <- readIORef mateCtx
  return $ ctxInterfaceMap ctx


setInterfaceMethodMap :: InterfaceMethodMap -> IO ()
setInterfaceMethodMap m = do
  ctx <- readIORef mateCtx
  writeIORef mateCtx $ ctx { ctxInterfaceMethodMap = m }

getInterfaceMethodMap :: IO InterfaceMethodMap
getInterfaceMethodMap = do
  ctx <- readIORef mateCtx
  return $ ctxInterfaceMethodMap ctx
