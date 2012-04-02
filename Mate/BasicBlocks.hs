{-# LANGUAGE OverloadedStrings #-}
module Mate.BasicBlocks where

import Data.Binary
import Data.Int
import qualified Data.Map as H
import System.Environment
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B

import JVM.Common
import JVM.ClassFile
import JVM.Converter
import JVM.Dump
import JVM.Assembler

import Debug.Trace

import Mate.Utilities

type Name       = String -- use "virtual register id" instead?
data Type       = JInt | JFloat -- add more
type Variable   = (Type,Name)

type BlockID = Int
-- Represents a CFG node
data BasicBlock = BasicBlock {
                     -- inputs  :: [Variable],
                     -- outputs :: [Variable],
                     code    :: [Instruction],
                     successor :: BBEnd }

-- describes (leaving) edges of a CFG node
data BBEnd = Return | OneTarget BlockID | TwoTarget BlockID BlockID deriving Show

type MapBB = H.Map BlockID BasicBlock

-- for immediate representation for determine BBs
type Offset = (Int, Maybe BBEnd) -- (offset in bytecode, offset to jump target)
type OffIns = (Offset, Instruction)


main = do
  args <- getArgs
  case args of
    [clspath] -> parseFile clspath "fib" -- TODO
    _ -> error "Synopsis: dump-class File.class"


parseFile :: String -> B.ByteString -> IO ()
parseFile clspath method = do
                     --clsFile <- decodeFile clspath
                     --putStrLn $ showListIx $ M.assocs $ constsPool (clsFile :: Class Pointers)
                     cls <- parseClassFile clspath
                     --dumpClass cls    
                     let mainmethod = lookupMethod method cls -- "main|([Ljava/lang/String;)V" cf
                     let hmap = testCFG mainmethod
                     putStr "BlockIDs: "
                     let keys = fst $ unzip $ H.toList hmap
                     mapM_ (putStr . (flip (++)) ", " . show) keys
                     putStrLn "\n\nBasicBlocks:"
                     printMapBB keys hmap

printMapBB :: [BlockID] -> MapBB -> IO ()
printMapBB [] _ = return ()
printMapBB (i:is) hmap = case H.lookup i hmap of
                Just bb -> do
                           putStrLn $ "Block " ++ (show i)
                           mapM_ putStrLn (map ((++) "\t" . show) $ code bb)
                           case successor bb of
                             Return -> putStrLn ""
                             OneTarget t1 -> putStrLn $ "Sucessor: " ++ (show t1) ++ "\n"
                             TwoTarget t1 t2 -> putStrLn $ "Sucessor: " ++ (show t1) ++ ", " ++ (show t2) ++ "\n"
                           printMapBB is hmap
                Nothing -> error $ "BlockID " ++ show i ++ " not found."

test_01 = parseFile "./tests/Fib.class" "fib"
test_02 = parseFile "./tests/While.class" "f"
test_03 = parseFile "./tests/While.class" "g"


testCFG :: Maybe (Method Resolved) -> MapBB
testCFG (Just m) = case attrByName m "Code" of
                     Nothing       -> error "no code"
                     Just bytecode -> let code = decodeMethod bytecode
                                          instructions = codeInstructions code
                                      in buildCFG instructions
testCFG _        = error "no method to build cfg"


buildCFG :: [Instruction] -> MapBB
buildCFG xs = buildCFG' H.empty xs' xs'
  where
  xs' :: [OffIns]
  xs' = calculateInstructionOffset xs

buildCFG' :: MapBB -> [OffIns] -> [OffIns] -> MapBB
buildCFG' hmap [] _ = hmap
buildCFG' hmap (((off, Just entry), _):xs) insns = buildCFG' (insertlist entryi hmap) xs insns
  where
  insertlist :: [BlockID] -> MapBB -> MapBB
  insertlist [] hmap = hmap
  insertlist (x:xs) hmap = insertlist xs newhmap
    where
    newhmap = if H.member x hmap then hmap else H.insert x value hmap
    value = parseBasicBlock x insns

  entryi :: [BlockID]
  entryi = (if off == 0 then [0] else []) ++ -- also consider the entrypoint
        case entry of
        TwoTarget t1 t2 -> [t1, t2]
        OneTarget t -> [t]
        Return -> trace "should not happen" []

buildCFG' hmap (((_, Nothing), _):xs) insns = buildCFG' hmap xs insns


parseBasicBlock :: Int -> [OffIns] -> BasicBlock
parseBasicBlock i insns = BasicBlock insonly endblock
  where
  startlist = dropWhile (\((x,_),_) -> x < i) insns
  (Just ((_,(Just endblock)),_), is) = takeWhilePlusOne validins startlist
  insonly = snd $ unzip is

  -- also take last (non-matched) element and return it
  takeWhilePlusOne :: (a -> Bool) -> [a] -> (Maybe a,[a])
  takeWhilePlusOne _ [] = (Nothing,[])
  takeWhilePlusOne p (x:xs)
    | p x       =  let (lastins, list) = takeWhilePlusOne p xs in (lastins, (x:list))
    | otherwise =  (Just x,[x])

  validins :: ((Int, Maybe BBEnd), Instruction) -> Bool
  validins ((_,x),_) = case x of Just _ -> False; Nothing -> True


calculateInstructionOffset :: [Instruction] -> [OffIns]
calculateInstructionOffset = cio' (0, Nothing)
  where
  newoffset :: Instruction -> Int -> Offset
  newoffset x off = (off + (fromIntegral $ B.length $ encodeInstructions [x]), Nothing)

  addW16Signed :: Int -> Word16 -> Int
  addW16Signed i w16 = i + (fromIntegral s16)
    where s16 = (fromIntegral w16) :: Int16

  cio' :: Offset -> [Instruction] -> [OffIns]
  cio' _ [] = []
  -- TODO(bernhard): add more instruction with offset (IF_ACMP, JSR, ...)
  cio' (off,_) (x:xs) = case x of
      IF _ w16 -> twotargets w16
      IF_ICMP _ w16 -> twotargets w16
      GOTO w16 -> onetarget w16
      IRETURN -> notarget
      _ -> ((off, Nothing), x):next
    where
    notarget = ((off, Just Return), x):next
    onetarget w16 = ((off, Just $ OneTarget $ (off `addW16Signed` w16)), x):next
    twotargets w16 = ((off, Just $ TwoTarget (off `addW16Signed` w16) (off + 3)), x):next
    next = cio' (newoffset x off) xs
