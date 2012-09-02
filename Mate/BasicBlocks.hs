{-# LANGUAGE OverloadedStrings #-}
module Mate.BasicBlocks(
  BlockID,
  BasicBlock,
  BBEnd,
  MapBB,
  Method,
  printMapBB,
  parseMethod,
  testCFG -- added by hs to perform benches from outside
  )where

import Data.Binary hiding (get)
import Data.Int
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Control.Monad.State
import Control.Applicative

import JVM.ClassFile
import JVM.Converter
import JVM.Assembler

import Mate.Types
import Mate.Debug
import Mate.Utilities

-- (offset in bytecode, offset to jump target, ins)
type OffIns = (Int, Maybe BBEnd, Instruction)

type Targets = [BlockID]
type BBState = Targets
type AnalyseState = State BBState [OffIns]


printMapBB :: MapBB -> IO ()
printMapBB hmap = do
  printfBb "BlockIDs: "
  let keys = M.keys hmap
  mapM_ (printfBb . flip (++) ", " . show) keys
  printfBb "\n\nBasicBlocks:\n"
  printMapBB' keys hmap
    where
      printMapBB' :: [BlockID] -> MapBB -> IO ()
      printMapBB' [] _ = return ()
      printMapBB' (i:is) hmap' = case M.lookup i hmap' of
        Just bb -> do
          printfBb $ "Block " ++ show i ++ "\n"
          mapM_ (printfBb . flip (++) "\n" . (++) "\t" . show) $ code bb
          printfBb $ case successor bb of
            Return -> ""
            FallThrough t1 -> "Sucessor: " ++ show t1 ++ "\n"
            OneTarget t1 -> "Sucessor: " ++ show t1 ++ "\n"
            TwoTarget t1 t2 -> "Sucessor: " ++ show t1 ++ ", " ++ show t2 ++ "\n"
          printMapBB' is hmap
        Nothing -> error $ "BlockID " ++ show i ++ " not found."

{-
testInstance :: String -> B.ByteString -> MethodSignature -> IO ()
testInstance cf method sig = do
  cls <- parseClassFile cf
  hmap <- parseMethod cls method sig
  printMapBB hmap

test_main :: IO ()
test_main = do
  test_01
  test_02
  test_03
  test_04

test_01, test_02, test_03, test_04 :: IO ()
test_01 = testInstance "./tests/Fib.class" "fib"
test_02 = testInstance "./tests/While.class" "f"
test_03 = testInstance "./tests/While.class" "g"
test_04 = testInstance "./tests/Fac.class" "fac"
-}


parseMethod :: Class Direct -> B.ByteString -> MethodSignature -> IO RawMethod
parseMethod cls methodname sig = do
  let method = fromMaybe
               (error $ "method " ++ (show . toString) methodname ++ " not found")
               (lookupMethodSig methodname sig cls)
  let codeseg = fromMaybe
                (error $ "codeseg " ++ (show . toString) methodname ++ " not found")
                (attrByName method "Code")
  let decoded = decodeMethod codeseg
  let mapbb = testCFG decoded
  let locals = fromIntegral (codeMaxLocals decoded)
  let stacks = fromIntegral (codeStackSize decoded)
  let codelen = fromIntegral (codeLength decoded)
  let methoddirect = methodInfoToMethod (MethodInfo methodname "" sig) cls
  let isStatic = methodIsStatic methoddirect
  let nametype = methodNameType methoddirect
  let argscount = methodGetArgsCount nametype + (if isStatic then 0 else 1)

  let msig = methodSignature method
  printfBb $ printf "BB: analysing \"%s\"\n" $ toString (methodname `B.append` ": " `B.append` encode msig)
  printMapBB mapbb
  -- small example how to get information about
  -- exceptions of a method
  -- TODO: remove ;-)
  let (Just m) = lookupMethodSig methodname sig cls
  case attrByName m "Code" of
    Nothing ->
      printfBb $ printf "exception: no handler for this method\n"
    Just exceptionstream ->
      printfBb $ printf "exception: \"%s\"\n" (show $ codeExceptions $ decodeMethod exceptionstream)
  return $ RawMethod mapbb locals stacks argscount codelen


testCFG :: Code -> MapBB
testCFG = buildCFG . codeInstructions

buildCFG :: [Instruction] -> MapBB
buildCFG xs = buildCFG' M.empty xs' xs'
  where
  xs' :: [OffIns]
  xs' = evalState (calculateInstructionOffset xs >>= markBackwardTargets) []

-- get already calculated jmp-targets and mark the predecessor of the
-- target-instruction as "FallThrough". we just care about backwards
-- jumps here (forward jumps are handled in buildCFG')
markBackwardTargets :: [OffIns] -> AnalyseState
markBackwardTargets [] = return []
markBackwardTargets (x:[]) = return [x]
markBackwardTargets (x@(x_off,x_bbend,x_ins):y@(y_off,_,_):xs) = do
  rest <- markBackwardTargets (y:xs)
  targets <- get
  let isTarget = y_off `elem` targets
      x_new = case x_bbend of
        Just _ -> x -- already marked, don't change
        Nothing -> if isTarget then checkX y_off else x
      checkX w16 = case x_bbend of
        Nothing -> (x_off, Just $ FallThrough w16, x_ins) -- mark previous insn
        _ -> error "basicblock: something is wrong"
  return $ x_new:rest



buildCFG' :: MapBB -> [OffIns] -> [OffIns] -> MapBB
buildCFG' hmap [] _ = hmap
buildCFG' hmap ((off, entry, _):xs) insns = buildCFG' (insertlist entryi hmap) xs insns
  where
    insertlist :: [BlockID] -> MapBB -> MapBB
    insertlist [] hmap' = hmap'
    insertlist (y:ys) hmap' = insertlist ys newhmap
      where
        newhmap = if M.member y hmap' then hmap' else M.insert y value hmap'
        value = parseBasicBlock y insns
    entryi :: [BlockID]
    entryi = if off == 0 then 0:ys else ys -- also consider the entrypoint
      where
        ys = case entry of
          Just (TwoTarget t1 t2) -> [t1, t2]
          Just (OneTarget t) -> [t]
          Just (FallThrough t) -> [t]
          Just Return -> []
          Nothing -> []


parseBasicBlock :: Int -> [OffIns] -> BasicBlock
parseBasicBlock i insns = BasicBlock insonly endblock
  where
    (lastblock, is) = takeWhilePlusOne validins omitins insns
    (_, _, insonly) = unzip3 is
    (_, Just endblock, _) = fromJust lastblock

    -- also take last (non-matched) element and return it
    takeWhilePlusOne :: (a -> Bool) -> (a -> Bool) -> [a] -> (Maybe a, [a])
    takeWhilePlusOne _ _ [] = (Nothing, [])
    takeWhilePlusOne p omit (x:xs)
      | omit x    = next
      | p x       = (\(ys, xs') -> (ys, x:xs')) next
      | otherwise = (Just x, [x])
      where
        next = takeWhilePlusOne p omit xs

    validins :: OffIns -> Bool
    validins (_, x, _) = isNothing x

    omitins :: OffIns -> Bool
    omitins (off, _, _) = off < i


calculateInstructionOffset :: [Instruction] -> AnalyseState
calculateInstructionOffset = cio' (0, Nothing, NOP)
  where
    addW16Signed :: Int -> Word16 -> Int
    addW16Signed i w16 = i + fromIntegral s16
      where s16 = fromIntegral w16 :: Int16

    cio' :: OffIns -> [Instruction] -> AnalyseState
    cio' _ [] = return $ []
    cio' (off,_,_) (x:xs) = case x of
        IF _ w16 -> twotargets w16
        IF_ICMP _ w16 -> twotargets w16
        IF_ACMP _ w16 -> twotargets w16
        IFNONNULL w16 -> twotargets w16
        IFNULL w16 -> twotargets w16
        GOTO w16 -> onetarget w16
        IRETURN -> notarget
        ARETURN -> notarget
        RETURN -> notarget
        _ -> ((off, Nothing, x):) <$> next
      where
        notarget = ((off, Just Return, x):) <$> next
        onetarget w16 = do
          let jump = off `addW16Signed` w16
          modify (jump:)
          ((off, Just $ OneTarget jump, x):) <$> next
        twotargets w16 = do
          let nojump = off + 3
          modify (nojump:)
          let jump = off `addW16Signed` w16
          modify (jump:)
          ((off, Just $ TwoTarget nojump jump, x):) <$> next
        next = cio' newoffset xs
        newoffset = (off + insnLength x, Nothing, NOP)

-- TODO(bernhard): does GHC memomize results? i.e. does it calculate the size
--                 of `NOP' only once?
insnLength :: Num a => Instruction -> a
insnLength = fromIntegral . B.length . encodeInstructions . (:[])
