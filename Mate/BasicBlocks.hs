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
import Data.List (foldl')
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


emptyBasicBlock :: BasicBlock
emptyBasicBlock = BasicBlock
                    { code = []
                    , bblength = 0
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
          printfBb $ "Block " ++ show i ++ ". len: " ++ (show $ bblength bb) ++ "\n"
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

  let exceptionMap :: ExceptionMap Word16
      exceptionMap = foldl' f M.empty $ codeExceptions decoded
        where
          f emap ce =
            if M.member key emap
              then M.adjust (value:) key emap
              else M.insert key [value] emap
              where
                key = (&&&) eStartPC eEndPC ce
                value = (&&&) g eHandlerPC ce
                  where
                    g ce' = case eCatchType ce' of
                        0 -> B.empty
                        x -> buildClassID cls x

  let msig = methodSignature method
  printfBb $ printf "BB: analysing \"%s\"\n" $ toString (methodname `B.append` ": " `B.append` encode msig)
  printMapBB mapbb
  return $ RawMethod mapbb exceptionMap locals stacks argscount codelen


testCFG :: Code -> MapBB
testCFG c = buildCFG (codeInstructions c) (codeExceptions c)
  where
    buildCFG :: [Instruction] -> [CodeException] -> MapBB
    buildCFG xs excps = execState (mapM buildCFG' $ alltargets ++ handlerEntries) M.empty
      where
      (offins, targets) = runState (calculateInstructionOffset tryBlocks xs) S.empty
      alltargets = S.toList $ S.insert 0 targets
      tryBlocks = map (fromIntegral . eStartPC) excps
      handlerEntries = map (fromIntegral . eHandlerPC) excps

      buildCFG' :: Int -> State MapBB ()
      buildCFG' off = do
        let value = parseBasicBlock off offins
        modify (M.insert off value)

parseBasicBlock :: Int -> [OffIns] -> BasicBlock
parseBasicBlock i insns = emptyBasicBlock
          { code = zip offsets insonly
          , bblength = lastoff - i + (insnLength lastins)
          , successor = endblock }
  where
    (lastblock, is) = takeWhilePlusOne validins omitins insns
    (offsets, _, insonly) = unzip3 is
    (lastoff, Just endblock, lastins) = fromJust lastblock

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


calculateInstructionOffset :: [BlockID] -> [Instruction] -> AnalyseState
calculateInstructionOffset exstarts = cio' 0
  where
    addW16Signed i w16 = i + fromIntegral s16
      where s16 = fromIntegral w16 :: Int16

    cio' :: Int -> [Instruction] -> AnalyseState
    cio' _ [] = return $ []
    cio' off (x:xs) = case x of
        IF _ w16 -> twotargets w16
        IF_ICMP _ w16 -> twotargets w16
        IF_ACMP _ w16 -> twotargets w16
        IFNONNULL w16 -> twotargets w16
        IFNULL w16 -> twotargets w16
        GOTO w16 -> onetarget w16
        ATHROW -> notarget
        IRETURN -> notarget
        ARETURN -> notarget
        RETURN -> notarget
        _ -> if newoffset `elem` exstarts
              then do
                modify (S.insert newoffset)
                ((off, Just $ OneTarget newoffset, x):) <$> next
              else normalins
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
        next = cio' newoffset xs
        newoffset = off + insLen
        insLen = insnLength x

-- TODO(bernhard): does GHC memomize results? i.e. does it calculate the size
--                 of `NOP' only once?
insnLength :: Num a => Instruction -> a
insnLength = fromIntegral . B.length . encodeInstructions . (:[])
