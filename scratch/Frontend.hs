module Frontend where

import Data.Int
import Data.Word
import Control.Monad.State
import Data.Maybe
import Data.List

import JVM.ClassFile
import JVM.Converter
import JVM.Dump
import JVM.Assembler 

import qualified JVM.Assembler as J
import qualified Data.Heap as H
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M


import Debug.Trace

import Graph

{- Thoughs on types and representations 
 - We start from constructing a CFG. What do we need here
 - ** Fast traversal which is aware of cycles
 - ** Fast successor, do we need predecessors?
 - ** Find all paths to current node (including back references)
 - ** Generic Node type in order to write mentioned operations
 -    generically. There should be no intermediate language "lock in"
 -    i.e. adding another IR should not kill CFG code
 -    Furthermore operations like SSA construction should
 -    not affect the CFG datastructure. Nodes contents should be 
 -    interchangable in a way.
 - ** Some form of unique naming - we would like to identify blocks
 -    and check whether code should be produced for this node
 - ** Should be Haskell idiomatic - should be composed with 
 -    standard haskell infrastructure
 - ** Convinient printing
 -
 - From this a inductive type should be appropriate?
 -
 -}


-- Means NumberedINSruction
type Size         = Int
type NInst        = (Source, Size, J.Instruction)
-- Putting Source and Target in data types prevents from interchanging ;-)
data Source       = Source Int deriving (Eq, Ord, Show)
data Target       = Target Int deriving (Eq, Ord, Show)
-- Source, Target Instruction id
type SourceTarget = (Source,Target)
-- State type for findContinuations
type ContState    = ([SourceTarget], H.MinPrioHeap Target Source)

data Block = BB [NInst] deriving (Show)
type CFG = G Block
data BlockID = Id Int deriving (Show,Ord,Eq)

type IndirectCFG = [([NInst],[Int],BlockID)] 

data FinalBlock = FinalBlock BlockID [NInst] deriving (Show)

instance Eq FinalBlock where
  FinalBlock (Id x) _  == FinalBlock (Id y) _ = x == y

instance Ord FinalBlock where
  FinalBlock (Id x) _ <= FinalBlock (Id y) _ = x <= y

less :: Target -> Source -> Bool
less (Target t) (Source s) = t < s

addW16Signed :: Int -> Word16 -> Int
addW16Signed i w16 = i + fromIntegral s16
     where s16 = fromIntegral w16 :: Int16

getInstOffsets :: [J.Instruction] -> [Int]
getInstOffsets = map (\i -> fromIntegral $ B.length $ J.encodeInstructions [i])

sumSeries'':: [Int] -> [Int]
sumSeries'' =  reverse . snd . foldl (\(s,xs) x -> (s+x,s : xs)) (0,[])

splitBlocksBackRef :: [NInst] -> [Int]
splitBlocksBackRef = concatMap checkSplit 
    where checkSplit inst = case inst of
                             (Source s, size, IF_ICMP _ t) -> []
                             (Source s, size, GOTO      t) -> [addW16Signed s t]
                             _                             -> []


getInstructions :: [Instruction] -> [(Source, Int, Instruction)]
getInstructions ins = zip3 (map Source $ sumSeries'' offsets) offsets ins
                      where offsets = getInstOffsets ins


getBranch :: NInst -> [Int]
getBranch (Source s, size, IF_ICMP _ t) = [addW16Signed s t, s+size]
getBranch (Source s, size, GOTO     t)  = [addW16Signed s t]
getBranch (Source s, size, IRETURN   )  = [0] --actually wrong
getBranch _                             = [] 

-- a version of Prelude.span whereby the first element
-- which does not hold predicate f is also included in 
-- the first list
spanPlus :: (a -> Bool) -> [a] -> ([a], [a])
spanPlus _ []     = ([],[])
spanPlus f (x:xs) = if f x then let (b, a) = spanPlus f xs
                                in (x:b, a)
                           else ([x],xs)

fromNInst :: NInst -> Int
fromNInst (Source s,_,_) = s

ifJust :: b -> Maybe a -> [b]
ifJust _ Nothing  = []
ifJust a (Just _) = [a]

splitBlocks :: [Int] -> [NInst] -> IndirectCFG
splitBlocks _        [] = []
splitBlocks backRefs xs = (map fst block, block >>= snd, Id id) : splitBlocks backRefs (map fst rest)
                          where getBranchAndRef ins@(Source i,w,_) = 
                                   (ifJust (i+w) $ find (==i+w) backRefs) ++ getBranch ins
                                branches     = zip xs $ map getBranchAndRef xs
                                (block,rest) = spanPlus (null . snd) branches
                                (Source id,_,_) = fst . head $ block -- block guarantted to be non empty

getTransitions :: IndirectCFG -> (M.Map BlockID [BlockID])
getTransitions = foldr (\(_,targets,id) s -> M.insert id (map Id targets) s) M.empty 

-- [([NInst],[Int],BlockID)] 
getNodes :: IndirectCFG -> [(BlockID,FinalBlock)]
getNodes = map rearrange
   where rearrange (insts,_,id) = (id,FinalBlock id insts)


--indirectCFGToG :: IndirectCFG -> G [NInst]
indirectCFGToG cfg = toG packNodes (M.toList $ getTransitions cfg) (Id 0) (getNodes cfg)

{- DEPRECATED -}

findContinuations :: [NInst] -> State ContState ()
findContinuations = mapM_ addCont

addCont :: NInst -> State ContState ()
addCont (s, b, IF_ICMP _ w) = do add s (addTarget s w); jmpNext s b;
addCont (s, _, GOTO      w) = add s (addTarget s w);
addCont (s, b, _         )  = return () --jmpNext s b

jmpNext :: Source -> Int -> State ContState ()
jmpNext s@(Source i) o = add s (Target $ i + o)


addTarget :: Source -> Word16 -> Target
addTarget (Source s) w16 = Target $ fromIntegral result
     where result =  s16 + fromIntegral w16 :: Int16
           s16    =  fromIntegral s :: Int16



addBranch :: Source -> Target -> State ContState ()
addBranch s t = do (branches,backRefs) <- get
                   put ( (s,t) : branches, backRefs)

addBackRef :: Source -> Target -> State ContState ()
addBackRef s t = do (branches,backRefs) <- get
                    put (branches, H.insert (t,s) backRefs) 

add :: Source -> Target -> State ContState ()
add s t = do addBranch s t
             -- if back branch - also add branch to back references
             -- for faster access later on
             if t `less` s then do trace ("jsadf") addBackRef s t; 
                           else return ()



-- Context of the Method to be compiled
type Context = ([J.Instruction],Class Resolved)
-- Maybe a context of the Method to be compiled
type MContext = Maybe Context
-- The state for CFG creation
type CFGState = (ContState, M.Map Int CFG)

type NodeState = ([(Source,Target)],[(Target,Source)])
type IndirectGraph k a = ((M.Map k a),(M.Map k [k]))





minNext ::  Ord t => [t] -> [t] -> ([t], [t], Maybe t)
minNext f@(x:xs) b@(y:ys) = if x < y then (xs, b, Just x) 
                                     else (f, ys, Just y)
minNext   (x:xs)   []     = (xs, [], Just x )
minNext   []       (y:ys) = ([], ys, Just y )
minNext   []       []     = ([], [], Nothing)

unpackst ((Source s), (Target t)) = (s,t)
unpackts ((Target t), (Source s)) = (t,s)

--createNodes :: [NInst] -> ContState -> IndirectGraph Int Block
--createNodes xs (forwardRefs, backwardRefs) = evalState (createNodes' xs) (branches, bRefs)
--   where branches = map reverse forwardRefs
--         bRefs    = map H.toAscList backwardRefs

--createNodes' ::[NInst] -> State NodeState (IndirectGraph Int Block)
{-createNodes' xs = do (st,ts) <- get
                     case (st,ts) of
                       -- there are back refs and forward refs
                       ((s,t):st', (t',s'):ts') 
                          -> if t' `less` s  
                              then do put (st,ts') -- back reference splits block
                                      let (Target entry, Source source) = (t',s')
                                      return (take entry xs, [])
                              else do put (st',ts) 
                                      let
                       _ -> undefined

-}
getCFG ::  FilePath -> B.ByteString -> IO (Maybe CFG)
getCFG file name = do context <- getMethodIO file name
                      return $ buildCFGContext context

buildCFGContext :: MContext -> Maybe CFG 
buildCFGContext = liftM genCFGPure 

genCFGPure :: Context -> CFG
genCFGPure (ins,cls) = let offsets    = getInstOffsets ins
                           taggedInst = zip3 (map Source $ sumSeries'' offsets) offsets ins
                           cont       = execState (findContinuations taggedInst) ([], H.empty)
                        in evalState (genCFGState taggedInst) (cont, M.empty)

methodCodeM :: B.ByteString-> Maybe (Class Resolved)-> Maybe (Class Resolved, B.ByteString)
methodCodeM name mcls = do cls <- mcls
                           ins <- methodCode cls name
                           return (cls,ins)

getMethod :: FilePath-> B.ByteString-> IO (Maybe (B.ByteString, Class Resolved))
getMethod file name = do cls <- parseClassFile file 
                         return (methodCode cls name >>= \ins -> return (ins,cls))

fullDecode ::  (B.ByteString, t) -> ([J.Instruction], t)
fullDecode (ins, cls) = (J.codeInstructions $ J.decodeMethod ins, cls)
 
getMethodIO :: FilePath-> B.ByteString-> IO (Maybe ([J.Instruction], Class Resolved))
getMethodIO file name = do context <- getMethod file name
                           return $ liftM fullDecode context


genCFGState = undefined


