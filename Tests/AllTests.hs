module Main where

import Mate.Tests.MockRefs

import Test.QuickCheck 

main = quickCheck testObjectTree
