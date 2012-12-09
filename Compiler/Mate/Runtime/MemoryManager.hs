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

import Compiler.Mate.Debug
import Compiler.Mate.Runtime.GC hiding (size)
import qualified Compiler.Mate.Runtime.StackTrace as T
import qualified Compiler.Mate.Runtime.JavaObjectsGC as GCObj
import Compiler.Mate.Runtime.JavaObjectsGC() -- only instances for Ptr a
import Compiler.Mate.Runtime.TwoSpaceAllocator
import Compiler.Mate.Runtime.GenerationalGC

type RootSet a = M.Map (Ptr a) RefUpdateAction


instance AllocationManager TwoSpace where
  initMemoryManager = initTwoSpace
  mallocBytesT = mallocBytes'
  performCollection = performCollection'
  
  heapSize = do space <- get
                return $ fromIntegral $ toHeap space - fromIntegral (toBase space)

  validRef _  = return True --liftM (validRef' ptr) get

printGc :: (Show a) => a -> IO ()
printGc = printfGc . show

performCollection' :: (RefObj a) => M.Map a RefUpdateAction -> StateT TwoSpace IO ()
performCollection' roots = do modify switchSpaces
                              let rootList = map fst $ M.toList roots
                              liftIO (printfGc "rootSet: " >> printGc rootList >> printfGc "\n")
                              performCollectionIO rootList
                              liftIO $ patchGCRoots roots

markedOrInvalid :: (RefObj a) => StateT TwoSpace IO (a -> IO Bool)
markedOrInvalid = do memManager <- get
                     return $ \obj -> do objAsPtr <- getIntPtr obj
                                         liftIO (printfGc $ printf "check obj: 0x%08x" (fromIntegral objAsPtr :: Int))
                                         let valid = True
                                         --let valid = validRef' objAsPtr memManager
                                         if valid && objAsPtr /= 0 
                                          then do validObj <- GCObj.validMateObj objAsPtr 
                                                  if validObj
                                                   then do m <- liftIO $ marked obj
                                                           liftIO (printfGc $ "markedOrinvalid: " ++ show m)
                                                           liftIO $ printRef obj
                                                           return m
                                                   else do 
                                                           liftIO $ printfGc "no taken2\n"
                                                           return True
                                          else do
                                                liftIO $ printfGc "not taken1\n"
                                                return True -- not valid reference


-- [todo hs] this is slow. merge phases to eliminate list with refs
performCollectionIO :: (RefObj a) => [a] -> StateT TwoSpace IO ()
performCollectionIO refs' = do 
  liftIO $ printfGc "Phase 1. Marking..\n"
  objFilter <- markedOrInvalid
  lifeRefs <- liftIO $ liftM (nub . concat) $ mapM (markTree'' objFilter mark refs') refs'
  liftIO $ printfGc "Done Phase 1.\n"
  liftIO $ mapM (getIntPtr >=> \x -> printfGc $ printf " 0x%08x" (fromIntegral x ::Int) ) lifeRefs
  liftIO $ printfGc "\nPhase 2. Evacuating...\n"
  evacuate' lifeRefs 
  lift $ printfGc "Phase 2. Done.\n"
  memoryManager <- get
  lift $ patchAllRefs (getIntPtr >=> \x -> return $ x /= 0) lifeRefs
  --lift $ patchAllRefs (getIntPtr >=> return . flip validRef' memoryManager) lifeRefs 
  lift $ printfGc "patched.\n"    

buildGCAction :: AllocationManager a => [T.StackDescription] -> Int -> StateT a IO (Ptr b)
buildGCAction [] size = mallocBytesT size
buildGCAction stack size = do let rootsOnStack = concatMap T.candidates stack --concatMap T.possibleRefs stack
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
                               poke (intPtrToPtr ptr) newLocation   
        ptr' = intPtrToPtr obj

        both newPatch oldPatch newLocation = do newPatch newLocation
                                                oldPatch newLocation
