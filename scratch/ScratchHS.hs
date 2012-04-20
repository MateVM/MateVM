{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Purpose of this file is just do test some Intermediate representations and stuff ;-)

{- Some important material:
 - 
 - Java HotSpot™ Client Compiler: www.cdl.uni-saarland.de/ssasem/talks/Christian.Wimmer.pdf
 - http://www.complang.tuwien.ac.at/andi/185A50
 - 
 - [Poletto:1999] http://dl.acm.org/citation.cfm?doid=330249.330250
 - [Wimmer:2010] http://dl.acm.org/citation.cfm?id=1772954.1772979
 -
 -}


module ScratchHS where

import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map)
import Control.Monad.State

import qualified Data.Heap as H
--import qualified Data.Heap as M 

import Harpy hiding(fst,add)
import qualified Harpy.X86Disassembler as H

import Foreign
import Control.Monad

import Debug.Trace
import Data.Int

import JVM.ClassFile
import JVM.Converter
import JVM.Dump

import qualified JVM.Assembler as J
import JVM.Assembler 

import Mate.Utilities
import Mate.BasicBlocks

import qualified Data.ByteString.Lazy as B


$(callDecl "callAsWord32" [t|Word32|])

data SimpleStack = PushLit Int
                 | Mul
                 | Add
                 | Ld String
                 | Print

testP = [PushLit 3, PushLit 2, Mul]

type Reg = Int 
data ROp = RMul | RAdd

data RegIL = RMov Reg Reg
           | RLoad Reg String 
           | RBin  Reg Reg Reg ROp

data MateState = MateState String

compileRegIL :: RegIL -> CodeGen (Ptr Int32) MateState ()
compileRegIL (RMov t s) = do 
                           mateState <- getState
                           let (mt,ms) = (eax,eax)
                           mov mt ms


entryCode :: CodeGen e s ()
entryCode = do push ebp
               mov ebp esp

exitCode :: CodeGen e s ()
exitCode = do mov esp ebp
              pop ebp 
              ret



run :: [RegIL] -> Ptr Int32 -> IO (MateState, Either ErrMsg [H.Instruction])
run program env = let compileAndFeedback = mapM_ compileRegIL program >> disassemble
                  in runCodeGen compileAndFeedback env (MateState "none")


-- Allocates a buffer with size n. All zero.
emptyMemory ::  (Storable a, Num a) => Int -> IO (Ptr a)
emptyMemory n = mallocArray n 
                  >>= (\ptr -> pokeArray ptr (replicate n 0) >> return ptr)


testEnv p' = do 
              ptr <- emptyMemory 26
              (_, Right code) <- run p' ptr
              return $ map H.showIntel code


simpleTest ::  [RegIL]
simpleTest = [RMov 0 1]


-- Just some class file sand
loadMethod methodName classFile = do cls <- parseClassFile classFile
                                     dumpClass cls
                                     return (cls, lookupMethod methodName cls)


getFib = do (cls, Just m) <- loadMethod "fac" "../tests/Fac.class"
            return (cls, m)

fibBasicBlocks = do (cls,m) <- getFib
                    hmap <- parseMethod cls "facFor"
                    printMapBB hmap
                    return ()


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

data G a = Node  a (G a) (G a)
         | Leaf  a 
         | Nil deriving(Show)


testG = [(0,[1,2]),(1,[3]),(2,[0]),(3,[])]
nodeLoads :: [(Int,String)]
nodeLoads = [(0,"a"),(1,"b"),(2,"c"),(3,"d")]

toG :: (Ord k, Show k) => [(k,[k])] -> k -> [(k, a)] -> Maybe (G a)
toG xs start pls = let nodeTable = M.fromList pls
                       payload s = fromJust $ M.lookup s nodeTable
                       
                       toBin xs pl = case xs of
                                      (x:y:[]) -> Node pl x y
                                      (x:[]  ) -> Node pl x Nil
                                      ([]    ) -> Leaf pl

                       conn = M.fromList $ map (
                                  \(f,ts) -> let pl t = node f (payload t) t
                                                 succ = map pl ts
                                            in (f, toBin succ (payload f))
                                ) xs
                       
                       node f p t = case M.lookup t conn of
                                          (Just x) -> x
                   in M.lookup start conn

type LoopCheck a = Set a


{- Actually i am not sure about the cata and algebra definition.
 -
 - check: http://dl.acm.org/citation.cfm?id=128035
 -}

type TreeAlgebra a r = (a -> r, r -> r -> r)

foldG :: TreeAlgebra a r -> r -> G a -> r
foldG (f,g) s (Leaf val)     = g (f val) s
foldG (f,g) s (Node val l r) = g (f val) $ g (foldG (f,g) s l) (foldG (f,g) s r)
                        
printG = foldG ((: []), (++)) []

loopCheck :: (Ord k) => G a -> (G a -> k) -> State (LoopCheck k) Bool
loopCheck g f = do state <- get
                   return $ Set.member (f g) state

addNode :: (Ord k) => k -> State (LoopCheck k) ()
addNode k = do s <- get 
               put $ Set.insert k s
               return ()

foldGM :: (Ord k) => TreeAlgebra a r -> (G a -> k) -> r -> G a -> State (LoopCheck k) r
foldGM _     _ s    Nil           = return s
foldGM (f,g) c s k@(Leaf val)     = loopCheck k c >>= \p -> if p then return s else return $ g (f val) s
foldGM (f,g) c s k@(Node val l r) = loopCheck k c >>= \p -> if p then return s else continue
                                    where t        = foldGM (f,g) c s
                                          self     = g (f val)
                                          continue = do addNode $ c k
                                                        left  <- t l
                                                        right <- t r
                                                        return $ self $ g left right

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



{- CFG generation
 - ......
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
type ContState    = (H.MinPrioHeap Target Source, H.MinPrioHeap Source Target)

findContinuations :: [NInst] -> State ContState ()
findContinuations = mapM_ addCont

addCont :: NInst -> State ContState ()
addCont (s, b, IF_ICMP _ w) = do add s (addTarget s w); jmpNext s b;
addCont (s, _, GOTO      w) = add s (addTarget s w);
addCont _                   = return ();

jmpNext :: Source -> Int -> State ContState ()
jmpNext s@(Source i) o = add s (Target $ i + o)


addTarget :: Source -> Word16 -> Target
addTarget (Source s) w16 = Target $ fromIntegral result
     where result =  s16 + fromIntegral w16 :: Int16
           s16    =  fromIntegral s :: Int16


addW16Signed :: Int -> Word16 -> Int
addW16Signed i w16 = i + fromIntegral s16
     where s16 = fromIntegral w16 :: Int16


add :: Source -> Target -> State ContState ()
add s t = do (targetSource,sourceTarget) <- get
             put (H.insert (t, s) targetSource, H.insert (s,t) sourceTarget)

getInstOffsets :: [J.Instruction] -> [Int]
getInstOffsets = map (\i -> fromIntegral $ B.length $ J.encodeInstructions [i])

sumSeries'':: [Int] -> [Int]
sumSeries'' =  reverse . snd . foldl (\(s,xs) x -> (s+x,s : xs)) (0,[])



data Block = BB [NInst] deriving (Show)
type CFG = G Block

-- Context of the Method to be compiled
type Context = ([J.Instruction],Class Resolved)
-- Maybe a context of the Method to be compiled
type MContext = Maybe Context
-- The state for CFG creation
type CFGState = (ContState, M.Map Int CFG)


getCFG ::  FilePath -> B.ByteString -> IO (Maybe CFG)
getCFG file name = do context <- getMethodIO file name
                      return $ buildCFGContext context

buildCFGContext :: MContext -> Maybe CFG 
buildCFGContext = liftM genCFGPure 

genCFGPure :: Context -> CFG
genCFGPure (ins,cls) = let offsets    = getInstOffsets ins
                           taggedInst = zip3 (map Source $ sumSeries'' offsets) offsets ins
                           cont       = execState (findContinuations taggedInst) (H.empty,H.empty)
                        in evalState (genCFGState taggedInst) (cont, M.empty)


joinM :: Maybe a -> Maybe b -> Maybe (a,b)
joinM Nothing   _       = Nothing
joinM _        Nothing  = Nothing
joinM (Just a) (Just b) = Just (a,b)

joinM' = uncurry joinM

leq :: Target -> Source -> Bool
leq (Target t) (Source s) = t <= s

getCont ::  State CFGState ContState
getCont = do ((ts,st),_) <- get
             return (ts,st)


takeTill :: Int -> [NInst] -> ([NInst],[NInst])
takeTill ref = span (\(Source loc,s,inst) -> loc < ref) 


writeBack :: ContState -> State CFGState ()
writeBack s = do (_,cls) <- get
                 put (s,cls)

from :: Int -> [NInst] -> [NInst]
from ref  = dropWhile (\(Source loc,s,inst) -> loc < ref)


data Branch = BackRef    Target Source
            | ForwardRef Source [Target] deriving (Show)



getForwardRef ts s' t' restForward restBack =
       do  writeBack (ts, restForward) -- remove forward ref from state
           -- maybe this one is a twoRef
           anotherOne <- getNextBranch
           case anotherOne of
             Just (ForwardRef s'' t'') 
	       -- If the next reference has the same source concat them 
	       -> if s'' == s' then return $ Just $ ForwardRef s' (t' : t'')
                               else return $ Just $ ForwardRef s' [t']
             Nothing -> --fail ("bahh" ++ show s' ++ show t')
                        return $ Just $ ForwardRef s' [t']

getNextBranch :: State CFGState (Maybe Branch)
getNextBranch = do ((ts,st),m) <- get
                   case joinM' (H.view ts, H.view st) of
                       -- Here we found the next front and the next back reference.
                       -- which one is earlier
                       Just ( ( (t,s), restBack), ( (s',t'), restForward) ) 
                          -> if t `leq` s
                               then -- back reference
                                 do writeBack (restBack, st) -- remove back reference from ref state
                                    return $ Just $ BackRef t s
                               else -- forward reference
                                 --getForwardRef ts s' t' restForward restBack
                                 do writeBack (ts,restForward)
                                    return $ Just $ ForwardRef s' [t']
                       Nothing -> return Nothing


genCFGState :: [NInst] -> State CFGState CFG
genCFGState xs = do nextBranch <- getNextBranch
                    case nextBranch of
                      Nothing            -> return $ Leaf $ BB xs
                      Just (BackRef (Target t) (Source s)) 
                        -> do 
                             let (previousBlock,rest) = takeTill t xs
                             (refs,m) <- get
                             let patched = let p = M.insert s c m
                                               c = evalState (genCFGState rest) (refs,p)
                                           in c 
                             return patched
                      Just (ForwardRef _ _)
                        -> do (_,m) <- get
                              let t = 4
                              fail "sjdf"
                              case M.lookup t m of
                                     Nothing -> return $ Leaf $ BB xs
                                     Just b  -> return (Node (BB xs) b Nil)
                      _  -> fail (show nextBranch) 
                           -- return $ Leaf $ BB xs
{-
genCFGState :: [NInst] -> State CFGState (CFG, M.Map Source CFG)
genCFGState xs = do (ts,st) <- getCont 
                    case joinM' (H.view ts, H.view st) of
                      Just ( ( (t, s), backRef), ( (s', t'), sndBranch) ) 
                         -> if t `leq` s then do writeBack (backRef,st);
                                                 runUntilBackRef (t , s )
                                         else runUntilBranch  (s', t') sndBranch ts
                      Nothing           
                         ->  runUntilExit 
          where 
             -- There is no back reference left. Run block until Control leaves.
             runUntilExit = fail "run until exit" --Nothing = return takeWhile (\(s,w,inst) -> undefined) undefined 
             
             -- In the program future a goto lands at this instruction. 
             runUntilBackRef (Target ref,_) = 
                    do
                      let (block,rest) = takeTill ref xs
                      (followUpBlock,_) <- genCFGState rest
                      let cfg = Node (BB block) followUpBlock Nil
                      return $ (cfg, M.empty)
             
             runUntilBranch (Source s,Target t) st ts = 
                    do 
                      let (block,rest) = takeTill (s+1) xs
                      -- check wheter this instruction branches also to another location
                      case H.view st of
                        Just ((Source sndBr, Target t),restBranches)
                            -> if sndBr == s then do writeBack (ts,restBranches)
                                                     twoBranch sndBr t block 
                                             else oneBranch block
                        Nothing -> oneBranch block
                     where twoBranch sndBranch t' block = 
                               do (left, _) <- genCFGState $ from t  xs
                                  (right,_) <- genCFGState $ from t' xs
                                  return $ (Node (BB block) left right, M.empty)
                           oneBranch block = return (Leaf (BB block), M.empty) --fail $ "oneBranch" ++ (show s) ++ " " ++ (show st)
-}

fib = do con@(Just (ins,cls)) <- getMethodIO "../tests/Fac.class" "facFor"
         let offsets = getInstOffsets ins
         let taggedInst = zip3 (map Source $ sumSeries'' offsets) offsets ins
         mapM_ print taggedInst
         let continuations =  execState (findContinuations taggedInst) (H.empty, H.empty)
         print continuations
         let cfg = buildCFGContext con
         print cfg
         return cfg 



diamant ::  G String
diamant = let start = Node "a" left right
              left  = Node "l" end Nil
              right = Node "r" end Nil
              end   = Node "g" start Nil
          in start

dag = Node "a" (Node "b" (Leaf "c") (Leaf "d")) (Node "b" (Leaf "c") (Leaf "d"))


value (Node val _ _) = Just val
value (Leaf val    ) = Just val
value Nil            = Nothing
                                          

printG' ::  Ord k => G k -> [k]
printG' g = evalState (foldGM ((: []), (++)) (\node -> let (Just v) = value node in v) [] g) Set.empty
