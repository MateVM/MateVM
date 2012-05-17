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
    | otherwise = show ret_a `compare` show ret_b
    where cmp_args = show args_a `compare` show args_b

instance Show MethodInfo where
  show (MethodInfo method c sig) =
    toString c ++ "." ++ toString method ++ "." ++ show sig



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
type StringMap = M.Map B.ByteString Word32


-- map "methodtable addr" to "classname"
-- we need that to identify the actual type
-- on the invokevirtual insn
type VirtualMap = M.Map Word32 B.ByteString


-- store each parsed Interface upon first loading
type InterfaceMap = M.Map B.ByteString (Class Resolved)

-- store offset for each <Interface><Method><Signature> pair
type InterfaceMethodMap = M.Map B.ByteString Word32


toString :: B.ByteString -> String
toString bstr = decodeString $ map (chr . fromIntegral) $ B.unpack bstr


-- those functions are for the "global map hax"
-- TODO(bernhard): other solution please
foreign import ccall "set_mate_context"
  set_mate_context :: Ptr () -> IO ()

foreign import ccall "get_mate_context"
  get_mate_context :: IO (Ptr ())

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

ctx2ptr :: MateCtx -> IO (Ptr ())
ctx2ptr ctx = do
  ptr <- newStablePtr ctx
  return $ castStablePtrToPtr ptr

ptr2ctx :: Ptr () -> IO MateCtx
ptr2ctx ptr = deRefStablePtr (castPtrToStablePtr ptr :: StablePtr MateCtx)


setMethodMap :: MethodMap -> IO ()
setMethodMap m = do
  ctx <- get_mate_context >>= ptr2ctx
  ctx2ptr ctx { ctxMethodMap = m } >>= set_mate_context

getMethodMap :: IO MethodMap
getMethodMap = do
  ctx <- get_mate_context >>= ptr2ctx
  return $ ctxMethodMap ctx


setTrapMap :: TrapMap -> IO ()
setTrapMap m = do
  ctx <- get_mate_context >>= ptr2ctx
  ctx2ptr ctx { ctxTrapMap = m } >>= set_mate_context

getTrapMap :: IO TrapMap
getTrapMap = do
  ctx <- get_mate_context >>= ptr2ctx
  return $ ctxTrapMap ctx


setClassMap :: ClassMap -> IO ()
setClassMap m = do
  ctx <- get_mate_context >>= ptr2ctx
  ctx2ptr ctx { ctxClassMap = m } >>= set_mate_context

getClassMap :: IO ClassMap
getClassMap = do
  ctx <- get_mate_context >>= ptr2ctx
  return $ ctxClassMap ctx


setVirtualMap :: VirtualMap -> IO ()
setVirtualMap m = do
  ctx <- get_mate_context >>= ptr2ctx
  ctx2ptr ctx { ctxVirtualMap = m } >>= set_mate_context

getVirtualMap :: IO VirtualMap
getVirtualMap = do
  ctx <- get_mate_context >>= ptr2ctx
  return $ ctxVirtualMap ctx


setStringMap :: StringMap -> IO ()
setStringMap m = do
  ctx <- get_mate_context >>= ptr2ctx
  ctx2ptr ctx { ctxStringMap = m } >>= set_mate_context

getStringMap :: IO StringMap
getStringMap = do
  ctx <- get_mate_context >>= ptr2ctx
  return $ ctxStringMap ctx


setInterfaceMap :: InterfaceMap -> IO ()
setInterfaceMap m = do
  ctx <- get_mate_context >>= ptr2ctx
  ctx2ptr ctx { ctxInterfaceMap = m } >>= set_mate_context

getInterfaceMap :: IO InterfaceMap
getInterfaceMap = do
  ctx <- get_mate_context >>= ptr2ctx
  return $ ctxInterfaceMap ctx


setInterfaceMethodMap :: InterfaceMethodMap -> IO ()
setInterfaceMethodMap m = do
  ctx <- get_mate_context >>= ptr2ctx
  ctx2ptr ctx { ctxInterfaceMethodMap = m } >>= set_mate_context

getInterfaceMethodMap :: IO InterfaceMethodMap
getInterfaceMethodMap = do
  ctx <- get_mate_context >>= ptr2ctx
  return $ ctxInterfaceMethodMap ctx
