{-# OPTIONS_GHC -fno-warn-orphans #-}
module Compiler.Mate.Runtime.GenerationalGC where

import Foreign
import qualified Foreign.Marshal.Alloc as Alloc
import Control.Monad.State
import qualified Data.Map as M
import Data.Map(Map,(!))
import qualified Data.Set as S
import Data.List

import Compiler.Mate.Runtime.BlockAllocation
import Compiler.Mate.Runtime.GC
import Compiler.Mate.Debug
import Compiler.Mate.Runtime.MemoryManager
import Compiler.Mate.Flags
import qualified Compiler.Mate.Runtime.StackTrace as T

maxGen :: Int
maxGen = 2 -- means 0,1,2

instance AllocationManager GcState where
    initMemoryManager = initGen
    mallocBytesT = mallocBytesGen
    performCollection = collectGen
    collectLoh = collectLohTwoSpace
    heapSize = error "heap size in GenGC not implemented"
    validRef = error "valid ref in GenGC not implemented"

initGen :: Int -> IO GcState
initGen size' = do 
                  freshAllocState <- if useCachedAlloc 
                                      then mkAllocC size'
                                      else mkAllocC 0
                  return  GcState { generations = foldr (\i m -> M.insert i (generation' i) m) M.empty [0..maxGen],
                                    allocs = 0,
                                    allocatedBytes = 0 ,
                                    loh = S.empty, 
                                    allocState = freshAllocState }
    where generation' i = GenState { freeBlocks = [], 
                                     activeBlocks = M.empty,
                                     collections = 0,
                                     generation = i }


mallocBytesGen :: GenInfo -> Int -> StateT GcState IO (Ptr b)
mallocBytesGen gen size' = 
    if size' > loThreshhold  
      then allocateLoh size'
      else do 
            ptr <- runBlockAllocatorC gen size' 
            current <- get 
            put $ current { allocs = 1 + allocs current }
            logGcT $ printf "object got: %s\n" (show ptr)
            return ptr

allocateLoh :: Int -> StateT GcState IO (Ptr b)
allocateLoh size' = do
    current <- get
    let currentLoh = loh current
    ptr <- liftIO $ Alloc.mallocBytes size'
    put $ current { loh = S.insert (ptrToIntPtr ptr) currentLoh }
    liftIO $ printfGc $ printf "LOH: allocated %d bytes in loh %s" size' (show ptr)
    return ptr

collectLohTwoSpace :: (RefObj a) => [a] -> StateT GcState IO ()
collectLohTwoSpace xs = do
    current <- get
    intptrs <- liftIO $ mapM getIntPtr xs
    let oldLoh = loh current
    let newSet = S.fromList intptrs
    let toRemove = oldLoh `S.difference` newSet
    liftIO $ printfGc $ printf "objs in loh: %d" (S.size oldLoh)
    liftIO $ printfGc $ printf "old loh: %s" (show $ showRefs $ S.toList oldLoh)
    liftIO $ printfGc $ printf "to remove: %s" (show $ showRefs $ S.toList toRemove) 
    liftIO $ mapM (free . intPtrToPtr) (S.toList toRemove)
    put current { loh = newSet }

-- given an element in generation x -> where to evaucuate to
sourceGenToTargetGen :: Int -> Int 
sourceGenToTargetGen 0 = 1
sourceGenToTargetGen 1 = 2
sourceGenToTargetGen 2 = 2
sourceGenToTargetGen x = error $ "source object is in strange generation: " ++ show x

collectGen :: (RefObj b) => Map b RefUpdateAction -> StateT GcState IO ()
collectGen roots = do
    cnt <- liftM allocs get
    performCollectionGen (calculateGeneration cnt) roots
    --performCollectionGen Nothing roots

calculateGeneration :: Int -> Maybe Int
calculateGeneration x | x < 20 = Nothing
                      | x < 50 = Just 0
                      | x < 60 = Just 1
                      | otherwise = Just 2

performCollectionGen :: (RefObj b) => Maybe Int -> Map b RefUpdateAction  -> StateT GcState IO ()
performCollectionGen Nothing _ = logGcT "skipping GC. not necessary atm. tune gc settings if required"
performCollectionGen (Just generation') roots = do
   current <- get
   put current { allocs = 0 }
   logGcT $ printf "!!! runn gen%d collection" generation'
   let rootList = map fst $ M.toList roots
   logGcT $ printf  "rootSet: %s\n " (show rootList)
   toKill <- performCollectionGen' generation' rootList
   logGcT "patch gc roots.."
   liftIO $ patchGCRoots roots
   logGcT "all done \\o/"
   if useCachedAlloc
     then freeGensIOC toKill 
     else liftIO $ freeGensIO toKill 


buildPatchAction :: [T.StackDescription] -> [IntPtr] -> IO (Map (Ptr b) RefUpdateAction)
buildPatchAction [] _ = return M.empty
buildPatchAction stack roots = do
       let rootsOnStack = roots ++ concatMap T.candidates stack 
       rootCandidates <- mapM dereference rootsOnStack
       let realRoots = filter ((/= 0) . snd) rootCandidates
       return $ foldr buildRootPatcher2 M.empty realRoots


buildRootPatcher2 :: (IntPtr,IntPtr) -> Map (Ptr b) RefUpdateAction -> Map (Ptr b) RefUpdateAction
buildRootPatcher2 (ptr,obj) = M.insertWith both ptr' patch 
  where patch newLocation = do printfGc $ printf "patch new ref: 0x%08x on stackloc: 0x%08x .. " 
                                 (fromIntegral newLocation :: Int) (fromIntegral ptr :: Int)
                               poke (intPtrToPtr ptr) newLocation  
                               printfPlain "=>patched.\n"
        ptr' = intPtrToPtr obj

        both newPatch oldPatch newLocation = do newPatch newLocation
                                                oldPatch newLocation

replaceIndices :: Eq a => [Int] -> Map Int a -> (Int -> a) -> Map Int a
replaceIndices indices m repl = foldr replace m indices
  where replace index = M.insert index (repl index) 

takeIndices :: Map Int a -> [Int] -> [a]
takeIndices xs = map (\x -> xs!x) 
 
switchStates :: Int -> StateT GcState IO [GenState]
switchStates collection = do
    let toBeReplaced = [0..collection] --all collections up to collection should be replaced by empty ones
    current <- get
    let gens = generations current
    let newGens = replaceIndices toBeReplaced gens mkGenState
    logGcT $ printf "new generations: %s" (show newGens)
    put current { generations = newGens }
    logGcT $ printf "generations to be killed: %s" (show toBeReplaced)
    return $ takeIndices gens toBeReplaced

performCollectionGen' :: (RefObj a) => Int -> [a] -> StateT GcState IO [GenState]
performCollectionGen' collection refs' = do 
  toKill <- switchStates collection
  logGcT "==>Phase 1. Marking..\n"
  objFilter <- markedOrInvalid
  allLifeRefs <- liftIO $ liftM (nub . concat) $ mapM (markTree'' objFilter mark refs') refs'
  logGcT "==>Done Phase 1.\n"
  toEvacuate <- liftIO $ filterM (getIntPtr >=> return . uglyFilter) allLifeRefs 
  if gcLogEnabled 
    then  liftIO $ mapM_ (getIntPtr >=> \x -> printfGc $ printf " 0x%08x" (fromIntegral x ::Int) ) toEvacuate
    else return ()
  (largeObjs,lifeRefs) <- liftIO $ extractLargeObjects toEvacuate
  logGcT "\nPhase 2. Evacuating...\n"
  evacuate' getRefInfo lifeRefs
  logGcT  "Phase 2. Done.\n"
  if useLoh
    then do 
            logGcT "killing unsued large objs\n"
            collectLoh largeObjs
            logGcT "cleaned up loh\n"
    else return ();
  liftIO $ patchAllRefs (getIntPtr >=> \x -> return $ x /= 0) lifeRefs
  logGcT "patched2.\n" 
  return toKill 


getRefInfo :: (RefObj a) => a -> IO GenInfo
getRefInfo obj = do
    intPtr <- getIntPtr obj
    let begin = shift (shift intPtr (-blockSizePowerOfTwo)) blockSizePowerOfTwo
    generation' <- peek (intPtrToPtr begin)
    printfGc $ printf "got a reference in generation: %d\n" generation'
    return GenInfo { targetGen = min 2 generation' }
