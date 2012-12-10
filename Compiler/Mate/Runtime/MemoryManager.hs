{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ExistentialQuantification #-}
module Compiler.Mate.Runtime.MemoryManager   
    ( AllocationManager(..)
    , buildGCAction )   where

import Foreign.Ptr
import Foreign.Storable

import Text.Printf
import Control.Monad.State
import qualified Data.Map as M
import Data.List

import Compiler.Mate.Flags
import Compiler.Mate.Debug
import Compiler.Mate.Runtime.GC hiding (size)
import qualified Compiler.Mate.Runtime.StackTrace as T
import qualified Compiler.Mate.Runtime.JavaObjectsGC as GCObj
import Compiler.Mate.Runtime.JavaObjectsGC() -- only instances for Ptr a
import Compiler.Mate.Runtime.TwoSpaceAllocator

type RootSet a = M.Map (Ptr a) RefUpdateAction


instance AllocationManager TwoSpace where
  initMemoryManager = initTwoSpace
  mallocBytesT = mallocBytes'
  performCollection = performCollection'

  collectLoh = collectLohTwoSpace
  
  heapSize = do space <- get
                return $ fromIntegral $ toHeap space - fromIntegral (toBase space)

  validRef _  = return True --liftM (validRef' ptr) get

performCollection' :: (RefObj a) => M.Map a RefUpdateAction -> StateT TwoSpace IO ()
performCollection' roots = do modify switchSpaces
                              let rootList = map fst $ M.toList roots
                              logGcT $ printf  "rootSet: %s\n " (show rootList)
                              performCollectionIO rootList
                              liftIO $ patchGCRoots roots
                              logGcT "all done \\o/"

markedOrInvalid :: (RefObj a, AllocationManager b) => StateT b IO (a -> IO Bool)
markedOrInvalid = 
  return $ \obj -> do objAsPtr <- getIntPtr obj
                      printfGc $ printf "check obj: 0x%08x" (fromIntegral objAsPtr :: Int)
                      --let valid = validRef' objAsPtr memManager
                      if uglyFilter objAsPtr-- this was not necassary before perm gens (now direct refs onto objs) 
                        then do validObj' <- GCObj.validMateObj objAsPtr 
                                if validObj'
                                 then do
                                        printfGc "gheck makred\n" 
                                        liftIO $ marked obj
                                 else do 
                                         printfGc "not valid1\n"
                                         return True
                        else do printfGc "not valid1\n"
                                return True -- not valid reference

uglyFilter :: IntPtr -> Bool
uglyFilter objAsPtr = objAsPtr /= 0 && objAsPtr /= 0x1228babe && objAsPtr /= 0x1227babe 

-- [todo hs] this is slow. merge phases to eliminate list with refs
performCollectionIO :: (RefObj a, AllocationManager b) => [a] -> StateT b IO ()
performCollectionIO refs' = do 
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

  if gcLogEnabled 
    then  liftIO $ mapM_ (getIntPtr >=> \x -> printfGc $ printf " 0x%08x" (fromIntegral x ::Int) ) lifeRefs
    else return ()
  evacuate' lifeRefs
  logGcT  "Phase 2. Done.\n"
  if useLoh
    then do 
            logGcT "killing unsued large objs\n"
            collectLoh largeObjs
            logGcT "cleaned up loh\n"
    else return ();
  liftIO $ patchAllRefs (getIntPtr >=> \x -> return $ x /= 0) lifeRefs
  --lift $ patchAllRefs (getIntPtr >=> return . flip validRef' memoryManager) lifeRefs 
  logGcT "patched2.\n"    


-- splits [a] into (large objects, normal objects)
extractLargeObjects :: RefObj a => [a] -> IO ([a],[a])
extractLargeObjects xs = 
    if not useLoh
      then return ([],xs)
      else do
            sizes <-  mapM (\x -> GCObj.size x >>= \s -> return (x,s)) xs
            let (lohs,objs) = partition ((>= loThreshhold) . snd) sizes
            return (map fst lohs, map fst objs)


buildGCAction :: AllocationManager a => [T.StackDescription] -> [IntPtr] -> Int -> StateT a IO (Ptr b)
buildGCAction [] _ size = mallocBytesT size
buildGCAction stack perm size = 
    do let rootsOnStack = perm ++ concatMap T.candidates stack --concatMap T.possibleRefs stack
       rootCandidates <- lift $ mapM dereference rootsOnStack
       realRoots <- filterM (notNullRef . snd) rootCandidates
       performCollection $ foldr buildRootPatcher M.empty realRoots
       mallocBytesT size
  where dereference :: IntPtr -> IO (IntPtr,IntPtr)
        dereference intPtr = do printfGc $ printf "deref stacklocation: 0x%08x\n" (fromIntegral intPtr :: Int)
                                obj <- peek $ intPtrToPtr intPtr :: IO IntPtr
                                printfGc $ printf "deref location: "
                                printfGc (show (intPtrToPtr obj) ++ "\n")
                                return (intPtr,obj)

-- (stackLocation,obj)
buildRootPatcher :: (IntPtr,IntPtr) -> RootSet a -> RootSet a
buildRootPatcher (ptr,obj) = M.insertWith both ptr' patch 
  where --patch = poke ptr' 
        patch newLocation = do printfGc $ printf "patch new ref: 0x%08x on stackloc: 0x%08x\n" 
                                 (fromIntegral newLocation :: Int) (fromIntegral ptr :: Int)
                               x <- poke (intPtrToPtr ptr) newLocation  
                               printfGc "died?" 
                               return x
        ptr' = intPtrToPtr obj

        both newPatch oldPatch newLocation = do newPatch newLocation
                                                oldPatch newLocation
