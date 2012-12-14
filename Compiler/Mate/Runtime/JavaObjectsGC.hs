{-# OPTIONS_GHC -fno-warn-orphans #-}
module Compiler.Mate.Runtime.JavaObjectsGC 
    ( RefObj(..)
    , printRef'
    , validMateObj
    , hasMTable
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
import Compiler.Mate.Runtime.ClassPool
import qualified Compiler.Mate.Types as T

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
  getNewRef ptr = do 
                     ptr' <- peekByteOff ptr newPtrOffset
                     if 1 == (fromIntegral $ ptrToIntPtr ptr' :: Int32) -- only marked (case for large obs)
                       then do printfGc "not movable obj\n"
                               return ptr
                       else return ptr'
  allocationOffset _ = 0

  validObj ptr = do 
                    objAsPtr <- getIntPtr ptr
                    return $ objAsPtr /= 0 && objAsPtr /= 0x1228babe && objAsPtr /= 0x1227b

  printRef    = printRef'


newPtrOffset ::  Int
newPtrOffset = T.objectGC

isArrayMagicNumber :: Int32 -> Bool
isArrayMagicNumber ptr = ptr == fromIntegral T.referenceArrayMagic || ptr == fromIntegral T.primitiveArrayMagic

hasMTable :: IntPtr -> Bool
hasMTable objAsPtr = objAsPtr /= 0 && objAsPtr /= fromIntegral T.primitiveArrayMagic 
                                    && objAsPtr /= fromIntegral T.referenceArrayMagic

-- [TODO hs] fix for array[array]
unpackRefs :: Ptr a -> IO [Ptr a]
unpackRefs ptr = do 
  isarray <- isArray ptr
  if isarray
    then do
      method_table <- peekByteOff ptr 0 :: IO Int32
      if method_table == fromIntegral T.referenceArrayMagic 
        then do len <- peekByteOff ptr 8 :: IO Int
                printfGc "got reference type array\n"
                peekArray len (ptr `plusPtr` 12)
        else do printfGc "got primitive array\n"
                return [] -- is array but primitives
    else do
      classPtr <- getClassNamePtr ptr
      fieldOffsets <- case classPtr of
           Just v -> 
             do fieldTypes <- getFieldTypes v
                printfGc $ printf "got types blubber %s with fields: %s\n" (show v) (show fieldTypes)
                let referenceFieldOffsets = filterReferenceFields fieldTypes
                return referenceFieldOffsets
           Nothing -> printfGc "could not get class name ptr" >> return []
      -- this is wrong - use reference types only here.
      -- numberOfFields <- getObjectFieldCountPtr ptr
      -- peekArray numberOfFields (ptr `plusPtr` fieldsOffset)
      mapM ( peek . plusPtr ptr) fieldOffsets

filterReferenceFields :: [(Int32, FieldSignature)] -> [Int]
filterReferenceFields = 
    map (fromIntegral . fst) . filter (isReferenceType . snd)


isArray :: Ptr a -> IO Bool
isArray ptr = do
    method_table <- peekByteOff ptr 0 :: IO Int32
    return $ isArrayMagicNumber method_table 

validMateObj :: IntPtr -> IO Bool
validMateObj intPtr = do let ptr = intPtrToPtr intPtr
                         isarray <- isArray ptr 
                         if isarray
                          then return True 
                          else do 
                                  clazzNameM <- getClassNamePtr ptr
                                  case clazzNameM of 
                                    Nothing -> do printfGc "invalid.\n" 
                                                  return False
                                    Just _ -> return True
                                  
getSize :: Ptr a -> IO Int
getSize ptr = do
  isarray <- isArray ptr
  if isarray
    then getArrayObjectSize ptr
    else getObjectSizePtr ptr

getArrayObjectSize :: Ptr a -> IO Int
getArrayObjectSize ptr = do
 len <- peekByteOff ptr 8 :: IO Int
 return $ 12 + len * 4

markedRef :: Ptr a -> IO Bool
markedRef ptr = liftM (/= (0::Int32)) (peekByteOff ptr T.objectGC  :: IO Int32)

markRef :: Int32 -> Ptr a -> IO ()
markRef val ptr = pokeByteOff ptr T.objectGC  val

setNewRefPtr :: Ptr a -> Ptr a -> IO ()
setNewRefPtr ptr = pokeByteOff ptr newPtrOffset 

patchRefsPtr :: Ptr a -> [Ptr a] -> IO ()
patchRefsPtr ptr xs = do
  isarray <- isArray ptr
  if isarray
    then pokeArray (ptr `plusPtr` T.arrayBase) xs 
    else pokeArray (ptr `plusPtr` T.objectField) xs

printInt32 :: String -> Int32 -> IO ()
printInt32 str ptr = printfGc $ printf str ptr

doLoop :: IO a
doLoop = doLoop

printRef' :: Ptr a -> IO ()
printRef' ptr = do 
    printInt32 "Print Obj: address 0x%08x\n" =<< (liftM fromIntegral (getIntPtr ptr) :: IO Int32)
    printInt32 "method_table: 0x%08x\n" =<< (peekByteOff ptr 0 :: IO Int32)
    method_table <- peekByteOff ptr 0 :: IO Int32
    
    let printObj = do
         printfGc $ printf "we got an object: %s" (show ptr)
         clazzNameM <- getClassNamePtr ptr 
         clazzName <- case clazzNameM of
                      Just v -> return v
                      Nothing -> do printfGc $ "getClassNamePtr called on non mate obj1: " ++ show ptr ++ "\n"
                                    printfGc $ printf "trying to dump on %s" (show ptr)
                                    mem <- peekArray 12 (castPtr ptr) :: IO [Ptr Int32]
                                    printfGc $ printf  "dump: %s" (show mem)
                                    doLoop
         printfGc $ printf "type: %s\n" $ toString clazzName
         fieldCnt <- getObjectFieldCountPtr ptr    
         printfGc $ printf "children 0x%08x\n" fieldCnt
         markedBit <- peekByteOff ptr T.objectGC :: IO Int32
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

