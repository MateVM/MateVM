{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
module GC where

import Control.Monad

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.C.Types
import GHC.Int
import Text.Printf

class RefObject a where
  mem  :: a -> IntPtr 
  refs :: a -> [IntPtr]


data RefObj = RefObj IntPtr [IntPtr] deriving Show

instance RefObject RefObj where
  mem  (RefObj mem _ ) = mem
  refs (RefObj _ refs) = refs

data Succ = forall a. (RefObject a) => Succ (a -> [a])

obj2 = do buffer <- mallocBytes 4
          pokeByteOff buffer 0 (0::Int32)
          return buffer

obj3 = do buffer <- mallocBytes 4
          pokeByteOff buffer 0 (0::Int32)
          return buffer

obj1 f g = do buffer <- mallocBytes 12
              pokeByteOff buffer 0 (2::Int32)
              pokeByteOff buffer 4 f
              pokeByteOff buffer 8 g
              return buffer

ptrToRefObj ptr = do objCount <- peek ptr :: IO Int32
                     let objsBase = ptr `plusPtr` 4
                     objs <- mapM ((liftM ptrToIntPtr) . peekElemOff objsBase . fromIntegral) [0..objCount-1]
                     return $ RefObj (ptrToIntPtr ptr) objs

test1 = do f <- obj2
           g <- obj3
           (print . ptrToIntPtr) f
           (print . ptrToIntPtr) g
           ptrToRefObj =<< obj1 f g

traverse :: (RefObject a) => (IntPtr -> IO a) -> a -> [a] -> IO [a]
traverse dereference x ws = do children <- mapM dereference (refs x)
                               undefined

succMem :: Ptr a -> Succ
succMem =undefined-- Succ (\_ -> obj1



