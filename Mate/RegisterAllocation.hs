{-# LANGUAGE CPP #-}
module Mate.RegisterAllocation where


#include "debug.h"

import Data.List
import Data.Maybe


type Label a = a
type IEdge a = (Label a, Label a)
data IGraph a = IGraph [IEdge a] deriving (Show)

-- TODO: make IEdge eq

--data IArchitecture = Arch { regs :: Integer  }
type IArchitecture = Int --number of regs


type Assignment a = (a, Int)


edgeEq (from,to) (from',to') = from == from' && to == to' 


-- TODO: find combinator do match try semantics here
-- Solution: use list because list is MonadPlus instance
-- other solution add maybe monadplus implementation
conflicts (IGraph xs) (label,anotherLabel) = let comparison  = edgeEq (label,anotherLabel)
                                                 comparison' = edgeEq (anotherLabel,label) 
                                             in isJust (find comparison xs) || isJust (find comparison' xs) 


isParticipiant label (from,to) = from == label || to == label

count p = length . filter p

degree g@(IGraph xs) label = count (isParticipiant label) xs


doChaitin81 :: (Eq a) => IArchitecture -> IGraph a -> [Assignment a]
doChaitin81 numberOfRegisters graph = []

type IState a = ([a],IGraph a)

--chait81Simplification :: (Eq a) => IArchitecture -> SimplState
--chait81Simplification regs = do (
                              
testGraph = IGraph [("a", "b"), ("a","c"), ("c", "b"), ("b","d"), ("d","c"),("b","e"),("d","e")]


