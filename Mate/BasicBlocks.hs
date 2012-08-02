{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
#include "debug.h"
module Mate.BasicBlocks(
  BlockID,
  BasicBlock,
  BBEnd,
  MapBB,
  Method,
#ifdef DBG_BB
  printMapBB,
#endif
  parseMethod,
  testCFG -- added by hs to perform benches from outside
  )where

import Data.Binary
import Data.Int
import Data.List
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Data.Maybe

import JVM.ClassFile
import JVM.Converter
import JVM.Assembler

import Mate.Types
import Mate.Debug
import Mate.Utilities

#ifdef DEBUG
import Text.Printf
#endif

-- for immediate representation to determine BBs
type Offset = (Int, Maybe BBEnd) -- (offset in bytecode, offset to jump target)
type OffIns = (Offset, Instruction)


#ifdef DBG_BB
printMapBB :: MapBB -> IO ()
printMapBB hmap = do
  putStr "BlockIDs: "
  let keys = M.keys hmap
  mapM_ (putStr . (flip (++)) ", " . show) keys
  putStrLn "\n\nBasicBlocks:"
  printMapBB' keys hmap
    where
      printMapBB' :: [BlockID] -> MapBB -> IO ()
      printMapBB' [] _ = return ()
      printMapBB' (i:is) hmap' = case M.lookup i hmap' of
        Just bb -> do
          putStrLn $ "Block " ++ (show i)
          mapM_ putStrLn (map ((++) "\t" . show) $ code bb)
          case successor bb of
            Return -> putStrLn ""
            FallThrough t1 -> putStrLn $ "Sucessor: " ++ (show t1) ++ "\n"
            OneTarget t1 -> putStrLn $ "Sucessor: " ++ (show t1) ++ "\n"
            TwoTarget t1 t2 -> putStrLn $ "Sucessor: " ++ (show t1) ++ ", " ++ (show t2) ++ "\n"
          printMapBB' is hmap
        Nothing -> error $ "BlockID " ++ show i ++ " not found."
#endif

#if 0
#ifdef DBG_BB
testInstance :: String -> B.ByteString -> MethodSignature -> IO ()
testInstance cf method sig = do
  cls <- parseClassFile cf
  hmap <- parseMethod cls method sig
  printMapBB hmap
#endif

#ifdef DBG_BB
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
#endif
#endif


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
  let methoddirect = methodInfoToMethod (MethodInfo methodname "" sig) cls
  let isStatic = methodIsStatic methoddirect
  let nametype = methodNameType methoddirect
  let argscount = methodGetArgsCount nametype + (if isStatic then 0 else 1)

  let msig = methodSignature $ classMethods cls !! 1
  printfBb "BB: analysing \"%s\"\n" $ toString (methodname `B.append` ": " `B.append` encode msig)
#ifdef DBG_BB
  printMapBB mapbb
#endif
  -- small example how to get information about
  -- exceptions of a method
  -- TODO: remove ;-)
  let (Just m) = lookupMethodSig methodname sig cls
  case attrByName m "Code" of
    Nothing ->
      printfBb "exception: no handler for this method\n"
    Just exceptionstream ->
      printfBb "exception: \"%s\"\n" (show $ codeExceptions $ decodeMethod exceptionstream)
  return $ RawMethod mapbb locals stacks argscount


testCFG :: Code -> MapBB
testCFG = buildCFG . codeInstructions

buildCFG :: [Instruction] -> MapBB
buildCFG xs = buildCFG' M.empty xs' xs'
  where
  xs' :: [OffIns]
  xs' = markBackwardTargets $ calculateInstructionOffset xs

-- get already calculated jmp-targets and mark the predecessor of the
-- target-instruction as "FallThrough". we just care about backwards
-- jumps here (forward jumps are handled in buildCFG')
markBackwardTargets :: [OffIns] -> [OffIns]
markBackwardTargets [] = []
markBackwardTargets (x:[]) = [x]
markBackwardTargets insns@(x@((x_off,x_bbend),x_ins):y@((y_off,_),_):xs) =
  x_new:markBackwardTargets (y:xs)
    where
      x_new = if isTarget then checkX y_off else x
      checkX w16 = case x_bbend of
        Just _ -> x -- already marked, don't change
        Nothing -> ((x_off, Just $ FallThrough w16), x_ins) -- mark previous insn

      -- look through all remaining insns in the stream if there is a jmp to `y'
      isTarget = case find cmpOffset insns of Just _ -> True; Nothing -> False
      cmpOffset ((_,Just (OneTarget w16)),_) = w16 == y_off
      cmpOffset ((_,Just (TwoTarget _ w16)),_) = w16 == y_off
      cmpOffset _ = False


buildCFG' :: MapBB -> [OffIns] -> [OffIns] -> MapBB
buildCFG' hmap [] _ = hmap
buildCFG' hmap (((off, entry), _):xs) insns = buildCFG' (insertlist entryi hmap) xs insns
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
    startlist = dropWhile (\((x,_),_) -> x < i) insns
    (Just ((_, Just endblock),_), is) = takeWhilePlusOne validins startlist
    insonly = snd $ unzip is

    -- also take last (non-matched) element and return it
    takeWhilePlusOne :: (a -> Bool) -> [a] -> (Maybe a,[a])
    takeWhilePlusOne _ [] = (Nothing,[])
    takeWhilePlusOne p (x:xs)
      | p x       =  let (lastins, list) = takeWhilePlusOne p xs in (lastins, x:list)
      | otherwise =  (Just x,[x])

    validins :: ((Int, Maybe BBEnd), Instruction) -> Bool
    validins ((_,x),_) = case x of Just _ -> False; Nothing -> True


calculateInstructionOffset :: [Instruction] -> [OffIns]
calculateInstructionOffset = cio' (0, Nothing)
  where
    newoffset :: Instruction -> Int -> Offset
    newoffset x off = (off + fromIntegral (B.length $ encodeInstructions [x]), Nothing)

    addW16Signed :: Int -> Word16 -> Int
    addW16Signed i w16 = i + fromIntegral s16
      where s16 = fromIntegral w16 :: Int16

    cio' :: Offset -> [Instruction] -> [OffIns]
    cio' _ [] = []
    -- TODO(bernhard): add more instruction with offset (IF_ACMP, JSR, ...)
    cio' (off,_) (x:xs) = case x of
        IF _ w16 -> twotargets w16
        IF_ICMP _ w16 -> twotargets w16
        IF_ACMP _ w16 -> twotargets w16
        IFNONNULL w16 -> twotargets w16
        IFNULL w16 -> twotargets w16
        GOTO w16 -> onetarget w16
        IRETURN -> notarget
        ARETURN -> notarget
        RETURN -> notarget
        _ -> ((off, Nothing), x):next
      where
        notarget = ((off, Just Return), x):next
        onetarget w16 = ((off, Just $ OneTarget (off `addW16Signed` w16)), x):next
        twotargets w16 = ((off, Just $ TwoTarget (off + 3) (off `addW16Signed` w16)), x):next
        next = cio' (newoffset x off) xs
