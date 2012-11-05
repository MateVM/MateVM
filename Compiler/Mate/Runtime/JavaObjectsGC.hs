{-# OPTIONS_GHC -fno-warn-orphans #-}
module Compiler.Mate.Runtime.JavaObjectsGC 
    ( RefObj(..)
    , printRef'
    , validMateObj
    ) where

import Compiler.Mate.Runtime.GC

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import GHC.Int

import Text.Printf

import Control.Monad
import JVM.ClassFile
import Compiler.Mate.Runtime.JavaObjects
import Compiler.Mate.Debug

instance RefObj (Ptr a) where
  getIntPtr   = return . ptrToIntPtr
  size        = getSize
  refs        = unpackRefs 
  marked      = markedRef
  mark        = markRef (0x1::Int32)
  unmark      = markRef (0x0::Int32)
  setNewRef   = setNewRefPtr
  patchRefs   = patchRefsPtr
  cast = castPtr
  getNewRef ptr = peekByteOff ptr newPtrOffset
  allocationOffset _ = 0

  printRef    = printRef'


markByteOffset, newPtrOffset, fieldsOffset ::  Int

markByteOffset = 4
newPtrOffset = 4
fieldsOffset = 8

fieldsOffsetArray :: Int
fieldsOffsetArray = 12

arrayMagicNumber :: Int32
arrayMagicNumber = 0x1227babe 

primArrayMagicNumber :: Int32
primArrayMagicNumber = 0x1228babe 

isArrayMagicNumber :: Int32 -> Bool
isArrayMagicNumber ptr = ptr == arrayMagicNumber || ptr == primArrayMagicNumber

-- [TODO hs] fix for array[array]
unpackRefs :: Ptr a -> IO [Ptr a]
unpackRefs ptr = do 
  isarray <- isArray ptr
  if isarray
    then do
      method_table <- peekByteOff ptr 0 :: IO Int32
      if method_table == arrayMagicNumber 
        then do len <- peekByteOff ptr 8 :: IO Int
                printfGc "got reference type array\n"
                peekArray len (ptr `plusPtr` 12)
        else do printfGc "got primitive array\n"
                return [] -- is array but primitives
    else do
      --print "ooohuh"
      numberOfFields <- getObjectFieldCountPtr ptr
      --print "got ooohuh"
      peekArray numberOfFields (ptr `plusPtr` fieldsOffset)

isArray :: Ptr a -> IO Bool
isArray ptr = do
    printfGc "isArray..\n"
    method_table <- peekByteOff ptr 0 :: IO Int32
    printfGc "done array. \n"
    return $ isArrayMagicNumber method_table 

validMateObj :: IntPtr -> IO Bool
validMateObj intPtr = do let ptr = intPtrToPtr intPtr
                         isarray <- isArray ptr 
                         if isarray
                          then return True 
                          else do printfGc "check mate obj\n"
                                  method_table <- peekByteOff ptr 0 :: IO (Ptr a)
                                  printfGc "done checking.\n"
                                  clazzNameM <- getClassNamePtr method_table
                                  case clazzNameM of 
                                    Nothing -> return False
                                    Just _ -> return True
                                  
getSize :: Ptr a -> IO Int
getSize ptr = do
  isarray <- isArray ptr
  if isarray
    then getArrayObjectSize ptr
    else do 
           --print "knobbl"
           s <- getObjectSizePtr ptr
           --print "got knobbl"
           return s

getArrayObjectSize :: Ptr a -> IO Int
getArrayObjectSize ptr = do
 len <- peekByteOff ptr 8 :: IO Int
 return $ 12 + len * 4

markedRef :: Ptr a -> IO Bool
markedRef ptr = liftM (/= (0::Int32)) (peekByteOff ptr markByteOffset  :: IO Int32)

markRef :: Int32 -> Ptr a -> IO ()
markRef val ptr = pokeByteOff ptr markByteOffset  val

setNewRefPtr :: Ptr a -> Ptr a -> IO ()
setNewRefPtr ptr = pokeByteOff ptr newPtrOffset 

patchRefsPtr :: Ptr a -> [Ptr a] -> IO ()
patchRefsPtr ptr xs = do
  isarray <- isArray ptr
  if isarray
    then pokeArray (ptr `plusPtr` fieldsOffsetArray) xs 
    else pokeArray (ptr `plusPtr` fieldsOffset) xs

printInt32 :: String -> Int32 -> IO ()
printInt32 str ptr = printfGc $ printf str ptr

printRef' :: Ptr a -> IO ()
printRef' ptr = do 
    printInt32 "Obj: address 0x%08x\n" =<< (liftM fromIntegral (getIntPtr ptr) :: IO Int32)
    printInt32 "method_table: 0x%08x\n" =<< (peekByteOff ptr 0 :: IO Int32)
    method_table <- peekByteOff ptr 0 :: IO Int32
    
    let printObj = do
         printfGc $ printf "we got an object"
         clazzName <- liftM (checkNothing "getClassNamePtr called on non mate obj") $ getClassNamePtr ptr 
         printfGc $ printf "type: %s\n" $ toString clazzName
         fieldCnt <- getObjectFieldCountPtr ptr    
         printfGc $ printf "children 0x%08x\n" fieldCnt
         markedBit <- (peekByteOff ptr markByteOffset :: IO Int32)           
         printInt32 "marked 0x%08x\n" markedBit
         printInt32 "newRef 0x%08x\n" =<< (peekByteOff ptr newPtrOffset :: IO Int32)
         printChildren ptr
         printfGc "\n"
  
    let printArray = do
        printfGc $ printf "we got an array\n"
        len <- peekByteOff ptr 8 :: IO Int32
        printfGc $ printf "length: 0x%08x\n" len
        printChildren ptr

    if isArrayMagicNumber method_table
      then printArray
      else printObj

printChildren :: Ptr a -> IO ()
printChildren ptr = do children <- refs ptr
                       printfGc $ "children" ++ show children

