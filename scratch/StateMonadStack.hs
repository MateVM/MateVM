module StateMonadStack where

import qualified Compiler.Hoopl as H
import Control.Monad.State
import qualified Data.Map as M

import Data.Char

type LabelMap = M.Map Char H.Label
type StackSim = [Int]

type LabelM m = StateT LabelMap m
type LabelSimM m a = StateT StackSim (LabelM m) a
type UniqueLabelSim a = LabelSimM H.SimpleUniqueMonad a


compile :: String -> UniqueLabelSim [(Int,H.Label)]
compile  = mapM compileCharacter

compileCharacter :: Char -> UniqueLabelSim (Int,H.Label)
compileCharacter char = do
 
  -- first simulate types etc
  stack <- get
  put $ ord char : stack
  
  -- look for label
  label <- lift $ getLabel char

  return (ord char, label)


getLabel :: Char -> LabelM H.SimpleUniqueMonad H.Label
getLabel x = do
  labels <- get

  -- lookup label. if exists return it otherwise create one
  case M.lookup x labels of
    Just v -> return v
    Nothing -> do fresh <- lift H.freshLabel
                  put $ M.insert x fresh labels
                  return fresh


test ::  IO ()
test = let mon = compile "aaabcda" 
           simulated = evalStateT mon []
           labelMapped = evalStateT simulated M.empty
           labelled = H.runSimpleUniqueMonad labelMapped
       in print labelled

