{-# OPTIONS_GHC -fno-warn-orphans #-}
module Mate.JavaObjectsGC 
    ( RefObj(..)
    , printRef'
    ) where

import Mate.GC

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import GHC.Int

import Text.Printf

import Control.Monad
import JVM.ClassFile
import Mate.JavaObjects

instance RefObj (Ptr a) where
  getIntPtr   = return . ptrToIntPtr
  size        = getObjectSizePtr
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

unpackRefs :: Ptr a -> IO [Ptr a]
unpackRefs ptr = do 
  numberOfFields <- getObjectFieldCountPtr ptr
  allRefs <- peekArray numberOfFields (ptr `plusPtr` fieldsOffset)
  -- there may me null reference stuff. 0s are no valid children
  return $ filter (/=nullPtr) allRefs


markedRef :: Ptr a -> IO Bool
markedRef ptr = liftM (/= (0::Int32)) (peekByteOff ptr markByteOffset  :: IO Int32)

markRef :: Int32 -> Ptr a -> IO ()
markRef val ptr = pokeByteOff ptr markByteOffset  val

setNewRefPtr :: Ptr a -> Ptr a -> IO ()
setNewRefPtr ptr = pokeByteOff ptr newPtrOffset 

patchRefsPtr :: Ptr a -> [Ptr a] -> IO ()
patchRefsPtr ptr = pokeArray (ptr `plusPtr` fieldsOffset) 


printRef' :: Ptr a -> IO ()
printRef' ptr = do 
    printf "Obj: address 0x%08x\n" =<< (liftM fromIntegral (getIntPtr ptr) :: IO Int32)
    printf "method_table: 0x%08x\n" =<< (peekByteOff ptr 0 :: IO Int32)
    className <- getClassNamePtr ptr 
    printf "type: %s\n" $ toString className
    printf "children 0x%08x\n" =<< getObjectFieldCountPtr ptr                 
    printf "marked 0x%08x\n" =<< (peekByteOff ptr markByteOffset :: IO Int32) 
    printf "newRef 0x%08x\n" =<< (peekByteOff ptr newPtrOffset :: IO Int32)
    printChildren ptr
    putStrLn ""

printChildren :: Ptr a -> IO ()
printChildren ptr = do children <- refs ptr
                       putStrLn $ "children" ++ show children

