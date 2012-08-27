{-# LANGUAGE ScopedTypeVariables #-}
module GC 
  ({- dont export generic versions for high performance ;-) -}) where

import Control.Monad

import Data.Foldable hiding (mapM_)

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import GHC.Int


class RefObj a where
  payload :: a -> IO IntPtr
  refs    :: a -> IO [a]
  marked  :: a -> IO Bool
  mark    :: a -> IO ()


instance RefObj (Ptr a) where
  payload     = return . ptrToIntPtr
  refs        = unpackRefs . castPtr
  marked      = markedRef
  mark        = markRef


idOff           = 0x0
numberOfObjsOff = 0x4
fieldsOff = 0xC
markedOff = 0x8

unpackRefs :: Ptr Int32 -> IO [Ptr b]
unpackRefs ptr = do  --dereference number of objs; mark field skipped via fieldsOffset
                    numberOfObjs <- peekByteOff ptr numberOfObjsOff :: IO Int32
                    mapM (peekElemOff (ptr `plusPtr` fieldsOff)) [0..fromIntegral $ numberOfObjs-1]

markedRef :: Ptr a -> IO Bool
markedRef ptr = liftM ((/=0) . fromIntegral) (peekByteOff ptr markedOff :: IO Int32)

markRef :: Ptr a -> IO ()
markRef ptr = pokeByteOff ptr markedOff (1::Int32)

markTree :: RefObj a => a -> IO ()
markTree root = marked root >>= (`when` continue) . not
  where continue = mark root >> refs root >>= mapM_  markTree


emptyObj id  = do mem <- mallocBytes 0xC
                  pokeArray mem [id,0,0::Int32]
                  return mem

twoRefs = do mem <- mallocBytes 0x14
             -- idOfObj; numberofObj; marked waste memory Int32
             pokeArray mem [0::Int32,2,0]
             obj1 <- emptyObj 1
             obj2 <- emptyObj 2
             pokeByteOff mem 0xC obj1
             pokeByteOff mem (0xC+0x4) obj2
             return mem


