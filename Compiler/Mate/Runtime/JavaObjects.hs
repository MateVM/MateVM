{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Compiler.Mate.Runtime.JavaObjects
  ( getUniqueStringAddr
  , allocAndInitObject
  , cloneObject
  , getObjectSizePtr
  , getObjectFieldCountPtr
  , getClassNamePtr
  , isReferenceType
  ) where

import Data.Word
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Internal as BI
import Control.Applicative
import Control.Monad

import JVM.ClassFile

import Foreign
import Foreign.C.Types

import Compiler.Mate.Backend.NativeSizes
import Compiler.Mate.Runtime.ClassPool
import {-# SOURCE #-} Compiler.Mate.Runtime.GarbageAlloc
import {-# SOURCE #-} Compiler.Mate.Runtime.MethodPool

import Compiler.Mate.Debug
import Compiler.Mate.Types

getUniqueStringAddr :: B.ByteString -> IO NativeWord
getUniqueStringAddr str = do
  smap <- getStringMap
  case M.lookup str smap of
    Nothing -> do
      addr <- allocateJavaString str
      setStringMap $ M.insert str addr smap
      return addr
    Just addr -> return addr

allocateJavaString :: B.ByteString -> IO NativeWord
allocateJavaString str = do
  {- we have to build a java object layout here, where String object looks like
   -
   -  this -+
   -        |
   -        v
   -  +-------------+---------+-------+-------+----------------+--------+
   -  | MethodTable | GC Data | value | count | cachedhashcode | offset |
   -  +-------------+---------+-------+-------+----------------+--------+
   -        |                     |
   -        |              +------+
   -        v              v         
   -  java/lang/String     +-------+--------+--------+--------+--------+-----+------------------+
   -                       | mtbl  | gcinfo | length | str[0] | str[1] | ... | str [length - 1] |
   -                       +-------+--------+--------+--------+--------+-----+------------------+
   -  all cells are 32bit wide, except str[i] of course (they're 8bit [but
   -  should be 16bit, TODO]).
   -  mtbl is a fake mtable as for all arrays, same for gcinfo.
   -}
  -- build object layout
  fsize <- getObjectSize "java/lang/String"
  printfStr $ printf "string: fsize: %d (should be 4 * 6)\n" fsize
  tblptr <- mallocObjectUnmanaged $ fromIntegral fsize
  let ptr = intPtrToPtr (fromIntegral tblptr) :: Ptr CPtrdiff
  mtbl <- getMethodTable "java/lang/String"

  -- build array layout
  let strlen = fromIntegral $ B.length str

  -- array objects have || method_table_fake | gc_data | length | ... \0 (for strings) ||
  -- [hs] i kept length + \0 and object correction separate for clarity 
  let arrayObjCorr = 2 * ptrSize 
  -- (+1) for \0, (+4) for length
  newstr <- mallocStringGC (strlen + 5 + arrayObjCorr)
  BI.memset newstr 0 (fromIntegral $ strlen + 5 + arrayObjCorr)
  -- set array fake mtbl and gcinfo
  poke (plusPtr newstr arrayMagic) primitiveArrayMagic 
  poke (plusPtr newstr arrayGC) (0x0 :: Word32) 
  arr <- newArray ((map fromIntegral $ B.unpack str) :: [Word8])
  copyBytes (plusPtr newstr (4 + arrayObjCorr)) arr strlen
  printfStr $ printf "new str ptr: (%s)@%d\n" (toString str) strlen

  poke (plusPtr newstr arrayLength) (fromIntegral strlen :: Int32)

  -- set mtable
  poke (plusPtr ptr objectMtable) (fromIntegral mtbl :: CPtrdiff)
  -- set GC Data (0 is ok for gc data)
  poke (plusPtr ptr objectGC) (0 :: CPtrdiff)
  -- set value pointer
  poke (plusPtr ptr 0x8) (fromIntegral (ptrToIntPtr newstr) :: CPtrdiff)
  -- set count field
  poke (plusPtr ptr 0xc) (fromIntegral strlen :: CPtrdiff)
  -- set hash code (TODO)
  poke (plusPtr ptr 0x10) (0 :: CPtrdiff)
  -- set offset
  poke (plusPtr ptr 0x14) (0 :: CPtrdiff)

  return $ fromIntegral tblptr


foreign import ccall "dynamic"
   code_ref :: FunPtr (CPtrdiff -> IO ()) -> CPtrdiff -> IO ()

allocAndInitObject :: B.ByteString -> IO CPtrdiff
allocAndInitObject p = do
  let mi = MethodInfo "<init>" p $ MethodSignature [] ReturnsVoid
  obj <- fromIntegral <$> getObjectSize p >>= mallocObjectGC
  let objptr = intPtrToPtr (fromIntegral obj)
  mtable <- getMethodTable p
  poke (plusPtr objptr objectMtable) mtable
  poke (plusPtr objptr objectGC) (0x1337babe :: CPtrdiff)
  entry <- lookupMethodEntry mi
  let fptr = (castPtrToFunPtr . intPtrToPtr . fromIntegral $ entry) :: FunPtr (CPtrdiff -> IO ())
  code_ref fptr obj
  return obj

-- [TODO hs] fix cloneObject
foreign export ccall cloneObject :: CPtrdiff -> IO CPtrdiff
cloneObject :: CPtrdiff -> IO CPtrdiff
cloneObject obj_to_clone = do
  let ptr = intPtrToPtr $ fromIntegral obj_to_clone :: Ptr NativeWord
  mtable <- peek ptr
  maybeObjTable <- getMethodTableReverse mtable
  case maybeObjTable of
    Nothing -> error "cloneObject performed <getMethodTableReverse> which returned Nothing."
    Just v  -> do size <- getObjectSize v
                  obj <- mallocObjectGC (fromIntegral size)
                  let objptr = intPtrToPtr (fromIntegral obj)
                  copyBytes objptr ptr (fromIntegral size)
                  return obj


{-nativeWordToPtr :: NativeWord -> Ptr a
nativeWordToPtr = intPtrToPtr . fromIntegral 

ptrToNativeWord :: Ptr a -> NativeWord
ptrToNativeWord = fromIntegral . ptrToIntPtr
-}

getClassNamePtr :: Ptr a -> IO (Maybe B.ByteString)
getClassNamePtr ptr = do
  method_table <- peek (castPtr ptr) :: IO Word32
  getMethodTableReverse method_table

getObjectSizePtr :: Ptr a -> IO Int
getObjectSizePtr ptr = do 
  clazzNameM <- getClassNamePtr ptr
  case clazzNameM of 
   Nothing -> error $ "getObjectSizePtr called on non mate object (getClassNamePtr returned Nothing)" ++ (show ptr)
   Just clazzName -> liftM fromIntegral $ getObjectSize clazzName

getObjectFieldCountPtr :: Ptr a -> IO Int
getObjectFieldCountPtr ptr = do 
  clazzNameM <- getClassNamePtr ptr
  case clazzNameM of 
   Nothing -> error "getObjectFieldCountPtr called on non mate object (getClassNamePtr returned Nothing)"
   Just clazzName -> liftM fromIntegral $ getFieldCount clazzName


isReferenceType :: FieldSignature -> Bool
isReferenceType (ObjectType _) = True
isReferenceType (Array _ _) = True
isReferenceType _ = False
