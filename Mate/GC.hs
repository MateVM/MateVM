{-# LANGUAGE ScopedTypeVariables #-}
module Mate.GC 
  ( RefObj(..), traverseIO, markTree'', markTree, patchAllRefs 
    {- dont export generic versions for high performance -> remove for production -}) where

import Control.Monad
import qualified Data.Set as S

import Foreign.Ptr (IntPtr, Ptr)

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

  printRef :: a -> IO ()
  

--class PrintableRef a where
--  printRef :: a -> IO ()

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
patchRefsObj :: (RefObj a) => a -> IO ()
patchRefsObj obj = do obj' <- getNewRef obj 
                      fields <- refs obj
                      newRefs <- mapM getNewRef fields
                      --print newRefs
                      patchRefs obj' newRefs                 

patchAllRefs :: (RefObj a) => [a] -> IO ()
patchAllRefs = mapM_ patchRefsObj


