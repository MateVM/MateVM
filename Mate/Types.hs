{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Mate.Types where

import Data.Char
import Data.Word
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Codec.Binary.UTF8.String hiding (encode,decode)

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.StablePtr

import JVM.ClassFile
import JVM.Assembler


type BlockID = Int
-- Represents a CFG node
data BasicBlock = BasicBlock {
                     code    :: [Instruction],
                     successor :: BBEnd }

-- describes (leaving) edges of a CFG node
data BBEnd = Return | FallThrough BlockID | OneTarget BlockID | TwoTarget BlockID BlockID deriving Show

type MapBB = M.Map BlockID BasicBlock


-- Word32 = point of method call in generated code
-- MethodInfo = relevant information about callee
type CMap = M.Map Word32 MethodInfo

-- B.ByteString = name of method
-- Word32 = entrypoint of method
type MMap = M.Map MethodInfo Word32

type ClassMap = M.Map B.ByteString ClassInfo

data ClassInfo = ClassInfo {
  clName :: B.ByteString,
  clFile :: Class Resolved }


data MethodInfo = MethodInfo {
  methName :: B.ByteString,
  cName :: B.ByteString,
  mSignature :: MethodSignature}

instance Eq MethodInfo where
  (MethodInfo m_a c_a s_a) == (MethodInfo m_b c_b s_b) =
    (m_a == m_b) && (c_a == c_b) && (s_a == s_b)

-- TODO(bernhard): not really efficient. also, outsource that to hs-java
instance Ord MethodSignature where
  compare (MethodSignature args_a ret_a) (MethodSignature args_b ret_b)
    | cmp_args /= EQ = cmp_args
    | otherwise = (show ret_a) `compare` (show ret_b)
    where
    cmp_args = (show args_a) `compare` (show args_b)

instance Ord MethodInfo where
  compare (MethodInfo m_a c_a s_a) (MethodInfo m_b c_b s_b)
    | cmp_m /= EQ = cmp_m
    | cmp_c /= EQ = cmp_c
    | otherwise = s_a `compare` s_b
    where
    cmp_m = m_a `compare` m_b
    cmp_c = c_a `compare` c_b

instance Show MethodInfo where
  show (MethodInfo method c sig) =
    (toString c) ++ "." ++ (toString method) ++ "." ++ (show sig)


toString :: B.ByteString -> String
toString bstr = decodeString $ map (chr . fromIntegral) $ B.unpack bstr


-- global map hax
foreign import ccall "get_callermap"
  get_callermap :: IO (Ptr ())

foreign import ccall "set_callermap"
  set_callermap :: Ptr () -> IO ()

foreign import ccall "get_methodmap"
  get_methodmap :: IO (Ptr ())

foreign import ccall "set_methodmap"
  set_methodmap :: Ptr () -> IO ()

foreign import ccall "get_classmap"
  get_classmap :: IO (Ptr ())

foreign import ccall "set_classmap"
  set_classmap :: Ptr () -> IO ()

-- TODO(bernhard): make some typeclass magic 'n stuff
mmap2ptr :: MMap -> IO (Ptr ())
mmap2ptr mmap = do
  ptr_mmap <- newStablePtr mmap
  return $ castStablePtrToPtr ptr_mmap

ptr2mmap :: Ptr () -> IO MMap
ptr2mmap vmap = deRefStablePtr $ ((castPtrToStablePtr vmap) :: StablePtr MMap)

cmap2ptr :: CMap -> IO (Ptr ())
cmap2ptr cmap = do
  ptr_cmap <- newStablePtr cmap
  return $ castStablePtrToPtr ptr_cmap

ptr2cmap :: Ptr () -> IO CMap
ptr2cmap vmap = deRefStablePtr $ ((castPtrToStablePtr vmap) :: StablePtr cmap)

classmap2ptr :: ClassMap -> IO (Ptr ())
classmap2ptr cmap = do
  ptr_cmap <- newStablePtr cmap
  return $ castStablePtrToPtr ptr_cmap

ptr2classmap :: Ptr () -> IO ClassMap
ptr2classmap vmap = deRefStablePtr $ ((castPtrToStablePtr vmap) :: StablePtr cmap)
