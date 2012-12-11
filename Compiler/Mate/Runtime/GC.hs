{-# LANGUAGE ScopedTypeVariables #-}
module Compiler.Mate.Runtime.GC 
  ( RefObj(..)
  , traverseIO
  , markTree'' 
  , markTree
  , patchAllRefs 
  , AllocationManager(..)
  , RefUpdateAction
  , notNullRef 
  , patchGCRoots
  , GenInfo(..)
  , mkGen0
  ) where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Printf
import Foreign.Ptr (IntPtr, Ptr)
import Compiler.Mate.Debug

class (Eq a, Ord a, Show a) => RefObj a where
  
  getIntPtr :: a -> IO IntPtr
  size      :: a -> IO Int
  cast      :: Ptr b -> a
 
  refs      :: a -> IO [a]
  patchRefs :: a -> [a] -> IO ()
  setNewRef :: a -> a -> IO ()
  getNewRef :: a -> IO a
  
  marked  :: a -> IO Bool
  mark    :: a -> IO ()
  unmark  :: a -> IO ()

  allocationOffset :: a -> Int

  validObj :: a -> IO Bool

  printRef :: a -> IO ()
  

type RefUpdateAction = IntPtr -> IO () -- the argument is the new location of the refobj

data GenInfo = GenInfo { targetGen :: Int }
mkGen0 :: GenInfo
mkGen0 = GenInfo { targetGen = 0 }

class AllocationManager a where
 
  initMemoryManager :: Int -> IO a

  -- | allocates n bytes in current space to space (may be to space or gen0 space)
  mallocBytesT :: GenInfo -> Int -> StateT a IO (Ptr b)
  
  -- | performs full gc and which is reflected in mem managers state
  performCollection :: (RefObj b) => M.Map b RefUpdateAction ->  StateT a IO ()

  -- | collects large objects (first argument is set of live objs)
  collectLoh :: RefObj b => [b] -> StateT a IO ()

  heapSize :: StateT a IO Int

  validRef :: IntPtr -> StateT a IO Bool

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
  where cont = foldM (\wsAcc x -> do let wsAcc' = S.insert x wsAcc
                                     traverseIO' f wsAcc' x) ws'
        ws' = S.insert root ws

markTree :: RefObj a => a -> IO ()
markTree root = marked root >>= (`unless` continue)
  where continue = mark root >> refs root >>= mapM_  markTree


-- | This object is alive. so its children are alive. patch child references to point
-- to childrens new references
patchRefsObj :: (RefObj a) => (a -> IO Bool) -> a -> IO ()
patchRefsObj predicate obj = do 
  intptr <- getIntPtr obj
  printfGc $ printf "patch 0x%08x" (fromIntegral intptr :: Int)
  printRef obj
  obj' <- getNewRef obj 
  fields <- refs obj
  printfGc "current fields:\n"
  printfGc $ show fields ++ "\n"
  newRefs <- mapM (getNewRefIfValid predicate) fields
  printfGc "this are the new children: "
  printfGc $ show newRefs ++ "\n"
  patchRefs obj' newRefs 
  patchRefs obj newRefs  
  printfGc "which is patched: \n"
  printRef obj'  

getNewRefIfValid :: (RefObj a) => (a -> IO Bool) -> a -> IO a
getNewRefIfValid predicate obj = do
  isValid <- predicate obj
  if isValid 
    then do newRef <- getNewRef obj
            newOneValid <- predicate newRef
            if newOneValid 
              then return newRef
              else return obj
    else do printfGc "geNewRefIfValid said obj itself is invalid. dafuqr\n"
            return obj

patchAllRefs :: (RefObj a) => (a -> IO Bool) -> [a] -> IO ()
patchAllRefs valid = mapM_ (patchRefsObj valid)

notNullRef :: AllocationManager a =>  IntPtr -> StateT a IO Bool
notNullRef = return . (/=(0x0 :: Int)) . fromIntegral
 
patchGCRoots :: (RefObj a) => M.Map a RefUpdateAction -> IO ()
patchGCRoots roots = mapM_ fixRef $ M.toList roots
  where fixRef (obj,fixupAction) = do valid <- validObj obj
                                      if valid 
                                        then getNewRef obj >>= getIntPtr >>= fixupAction
                                        else return ()


