module Mate.JavaObjectsGC 
    ( gcAllocationOffset
    , RefObj(..),
    ) where

import Mate.GC

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import GHC.Int

import Control.Monad

instance RefObj (Ptr a) where
  getIntPtr   = return . ptrToIntPtr
  size a      = fmap ((+ fieldsOffset) . (*4) . length) (refs a)
  refs        = unpackRefs 
  marked      = markedRef
  mark        = markRef (0x1::Int32)
  unmark      = markRef (0x0::Int32)
  setNewRef   = setNewRefPtr
  patchRefs   = patchRefsPtr
  cast = castPtr
  getNewRef ptr = peekByteOff ptr newPtrOffset

instance PrintableRef (Ptr a) where
  printRef    = undefined

gcAllocationOffset, markByteOffset, newPtrOffset, fieldsOffset, numberOfObjsOffset ::  Int
gcAllocationOffset = 12

markByteOffset = -8
newPtrOffset = -4
numberOfObjsOffset = 4
fieldsOffset = 8

unpackRefs :: Ptr a -> IO [Ptr a]
unpackRefs ptr = do 
  numberOfFields <- peekByteOff ptr numberOfObjsOffset
  peekArray numberOfFields (ptr `plusPtr` fieldsOffset)


markedRef :: Ptr a -> IO Bool
markedRef ptr = liftM (/= (0::Int32)) (peekByteOff ptr markByteOffset  :: IO Int32)

markRef :: Int32 -> Ptr a -> IO ()
markRef val ptr = pokeByteOff ptr markByteOffset  val

setNewRefPtr :: Ptr a -> Ptr a -> IO ()
setNewRefPtr ptr = pokeByteOff ptr newPtrOffset 

patchRefsPtr :: Ptr a -> [Ptr a] -> IO ()
patchRefsPtr ptr = pokeArray (ptr `plusPtr` fieldsOffset) 

