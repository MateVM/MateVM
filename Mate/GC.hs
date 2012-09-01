{-# LANGUAGE ScopedTypeVariables #-}
module Mate.GC 
  ( RefObj
    {- dont export generic versions for high performance -> remove for production -}) where

import Control.Monad

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import GHC.Int

import qualified Data.Set as S
import Text.Printf

class (Eq a, Ord a) => RefObj a where
  
  payload :: a -> IO IntPtr

  refs      :: a -> IO [a]
  patchRefs :: a -> [a] -> IO ()
  newRef    :: a -> a -> IO ()
  
  marked  :: a -> IO Bool
  mark    :: a -> IO ()
  unmark  :: a -> IO ()
  
  copy :: a -> IO a

class PrintableRef a where
  printRef :: a -> IO ()

instance RefObj (Ptr a) where
  payload     = return . ptrToIntPtr
  refs        = unpackRefs . castPtr
  marked      = markedRef
  mark        = markRef (0x1::Int32)
  unmark      = markRef (0x0::Int32)
  newRef      = newRefPtr
  patchRefs   = undefined
  copy = undefined

instance PrintableRef (Ptr a) where
  printRef    = printRef'


idOff           = 0x0
numberOfObjsOff = 0x4
markedOff = 0x8
newRefOff = 0xC
fieldsOff = 0x10

unpackRefs :: Ptr Int32 -> IO [Ptr b]
unpackRefs ptr = do  --dereference number of objs; mark field skipped via fieldsOffset
                    numberOfObjs <- peekByteOff ptr numberOfObjsOff :: IO Int32
                    mapM (peekElemOff (ptr `plusPtr` fieldsOff)) [0..fromIntegral $ numberOfObjs-1]

markedRef :: Ptr a -> IO Bool
markedRef ptr = liftM ((/=0) . fromIntegral) (peekByteOff ptr markedOff :: IO Int32)

markRef :: Int32 -> Ptr a -> IO ()
markRef val ptr = pokeByteOff ptr markedOff val

newRefPtr :: Ptr a -> Ptr a -> IO ()
newRefPtr ptr newRef = pokeByteOff ptr newRefOff newRef

printRef' :: Ptr a -> IO ()
printRef' ptr = do printf "obj 0x%08x\n" =<< (peekByteOff ptr idOff :: IO Int32)
                   printf "children 0x%08x\n" =<< (peekByteOff ptr numberOfObjsOff :: IO Int32)                  
                   printf "marked 0x%08x\n" =<< (peekByteOff ptr markedOff :: IO Int32)
                   printf "newRef 0x%08x\n\n" =<< (peekByteOff ptr newRefOff :: IO Int32)

-- | Generically marks a graph (can be used to set mark bit and reset mark bit at the same time
-- using customized loopcheck and marker funcs (i.e. to set the bit check on ==1 and on ==0 otherwise)
-- Furthermore it produces a list of visited nodes (this can be all live one (or dead on respectively)
markTree'' :: RefObj a => (a -> IO Bool) -> (a -> IO ()) -> [a] -> a -> IO [a]
markTree'' loopcheck marker ws root = do loop <- loopcheck root
                                         if loop then return ws else liftM (root :) continue
    where continue = marker root >> refs root >>= foldM (markTree'' loopcheck marker) ws

-- | For debugging only (implements custom loop check with Data.Set!)
traverseIO :: RefObj o => (o -> IO ()) -> o -> IO ()
traverseIO f = void . traverseIO' f S.empty

traverseIO' ::  RefObj a => (a -> IO ()) -> S.Set a -> a -> IO (S.Set a)
traverseIO' f ws root = if S.member root ws then f root >> return ws
                           else f root >> refs root >>= cont
  where cont = foldM (\ws x -> do let ws' = S.insert x ws
                                  traverseIO' f ws' x) ws'
        ws' = S.insert root ws

markTree :: RefObj a => a -> IO ()
markTree root = marked root >>= (`unless` continue)
  where continue = mark root >> refs root >>= mapM_  markTree

printTree :: Ptr a -> IO ()
printTree = traverseIO printRef'


emptyObj id  = do mem <- mallocBytes 0x10
                  pokeArray mem [id,0,0::Int32,0]
                  return mem

twoRefs = do mem <- mallocBytes 0x18
             -- idOfObj; numberofObj; marked waste memory Int32
             pokeArray mem [0::Int32,2,0,0]
             obj1 <- emptyObj 1
             obj2 <- emptyObj 2
             pokeByteOff mem 0x10 obj1
             pokeByteOff mem 0x14 obj2
             return mem

cyclR = do mem <- mallocBytes 0x1C
           pokeArray mem [0::Int32,3,0,0]
           obj1 <- emptyObj 1
           obj2 <- emptyObj 2
           pokeByteOff mem 0x10 obj1
           pokeByteOff mem 0x14 obj2
           pokeByteOff mem 0x18 mem
           return mem

test objr = do twoRefs <- objr
               putStrLn "initial:\n" 
               printTree twoRefs
               lifeRefs <- markTree'' marked mark [] twoRefs
               putStrLn "life refs: \n"
               print lifeRefs
               --forM lifeRefs printRef'
               putStrLn "after marking\n"
               printTree twoRefs
               markTree'' (liftM not . marked) unmark [] twoRefs
               putStrLn "after unmarking\n"
               printTree twoRefs
