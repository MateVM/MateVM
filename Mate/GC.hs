{-# LANGUAGE ScopedTypeVariables #-}
module Mate.GC 
  ( RefObj(..), PrintableRef(..), traverseIO, markTree'' 
    {- dont export generic versions for high performance -> remove for production -}) where

import Control.Monad
import qualified Data.Set as S

import Foreign.Ptr (IntPtr)

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

