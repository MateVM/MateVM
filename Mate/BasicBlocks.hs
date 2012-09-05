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
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Control.Monad.State
import Control.Applicative
import Control.Arrow

import JVM.ClassFile
import JVM.Converter
import JVM.Assembler

import Mate.Types
import Mate.Debug
import Mate.Utilities

-- (offset in bytecode, offset to jump target, ins)
type OffIns = (Int, Maybe BBEnd, Instruction)

type Target = BlockID
type BBState = S.Set Target
type AnalyseState = State BBState [OffIns]


noException :: B.ByteString
noException = B.empty

emptyBasicBlock :: BasicBlock
emptyBasicBlock = BasicBlock
                    { code = []
                    , exception = noException
                    , successor = Return }

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
buildCFG xs = execState (mapM (buildCFG' offins) alltargets) M.empty
  where
  (offins, targets) = runState (calculateInstructionOffset xs) S.empty
  alltargets = S.toList $ S.insert 0 targets

buildCFG' :: [OffIns] -> Int -> State MapBB ()
buildCFG' insns off = do
  let value = parseBasicBlock off insns
  modify (M.insert off value)

parseBasicBlock :: Int -> [OffIns] -> BasicBlock
parseBasicBlock i insns = emptyBasicBlock { code = insonly, successor = endblock }
  where
    (lastblock, is) = takeWhilePlusOne validins omitins insns
    (_, _, insonly) = unzip3 is
    (_, Just endblock, _) = fromJust lastblock

    -- also take last (non-matched) element and return it
    takeWhilePlusOne :: (a -> Bool) -> (a -> Bool) -> [a] -> (Maybe a, [a])
    takeWhilePlusOne _ _ [] = (Nothing, [])
    takeWhilePlusOne p omit (x:xs)
      | omit x    = next
      | p x       = second (x:) next
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
        _ -> normalins
      where
        normalins = do
          tailinsns <- next -- eval remaining instructions
          isNextInsATarget <- (S.member newoffset) <$> get
          let bbtyp = if isNextInsATarget
                then Just $ FallThrough newoffset
                else Nothing
          return $ (off, bbtyp, x):tailinsns
        notarget = ((off, Just Return, x):) <$> next
        onetarget w16 = do
          let jump = off `addW16Signed` w16
          modify (S.insert jump)
          ((off, Just $ OneTarget jump, x):) <$> next
        twotargets w16 = do
          let nojump = off + 3
          modify (S.insert nojump)
          let jump = off `addW16Signed` w16
          modify (S.insert jump)
          ((off, Just $ TwoTarget nojump jump, x):) <$> next
        next = cio' nextins xs
        nextins = (newoffset, Nothing, NOP)
        newoffset = off + insnLength x

-- TODO(bernhard): does GHC memomize results? i.e. does it calculate the size
--                 of `NOP' only once?
insnLength :: Num a => Instruction -> a
insnLength = fromIntegral . B.length . encodeInstructions . (:[])
