{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ExistentialQuantification #-}
module Compiler.Mate.Runtime.MemoryManager   
    ( AllocationManager(..)
    , TwoSpace(..)
    , initTwoSpace
    , mallocBytes'
    , RefUpdateAction
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
import qualified Compiler.Mate.Runtime.GC as GC
import Compiler.Mate.Runtime.TwoSpaceAllocator

type RootSet a = M.Map (Ptr a) RefUpdateAction


instance AllocationManager TwoSpace where
  mallocBytesT = mallocBytes'
  performCollection = performCollection'
  
  heapSize = do space <- get
                return $ fromIntegral $ toHeap space - fromIntegral (toBase space)

  validRef ptr = liftM (validRef' ptr) get

printGc :: (Show a) => a -> IO ()
printGc = printfGc . show

performCollection' :: (RefObj a) => M.Map a RefUpdateAction -> StateT TwoSpace IO ()
performCollection' roots = do modify switchSpaces
                              let rootList = map fst $ M.toList roots
                              liftIO (printfGc "rootSet: " >> printGc rootList >> printfGc "\n")
                              performCollectionIO rootList
                              liftIO $ patchGCRoots roots

patchGCRoots :: (RefObj a) => M.Map a RefUpdateAction -> IO ()
patchGCRoots roots = mapM_ fixRef $ M.toList roots
  where fixRef (obj,fixupAction) = getNewRef obj >>= getIntPtr >>= fixupAction


markedOrInvalid :: (RefObj a) => StateT TwoSpace IO (a -> IO Bool)
markedOrInvalid = do memManager <- get
                     return $ \obj -> do objAsPtr <- getIntPtr obj
                                         let valid = validRef' objAsPtr memManager
                                         if valid && objAsPtr /= 0 
                                          then do validObj <- GCObj.validMateObj objAsPtr 
                                                  if validObj
                                                   then liftM not (marked obj)
                                                   else return True
                                          else return True -- not valid reference


-- [todo hs] this is slow. merge phases to eliminate list with refs
performCollectionIO :: RefObj a => [a] -> StateT TwoSpace IO ()
performCollectionIO refs' = do 
  liftIO $ printfGc "before mark\n"
  objFilter <- markedOrInvalid
  lifeRefs <- liftIO $ liftM (nub . concat) $ mapM (markTree'' objFilter mark refs') refs'
  liftIO $ printfGc "marked\n"
  liftIO $ mapM printRef lifeRefs
  liftIO $ printfGc "go evacuate!\n"
  evacuate' lifeRefs 
  lift $ printfGc "eacuated. patching..\n"
  memoryManager <- get
  lift $ patchAllRefs (getIntPtr >=> return . flip validRef' memoryManager) lifeRefs 
  lift $ printfGc "patched.\n"    

buildGCAction :: AllocationManager a => [T.StackDescription] -> Int -> StateT a IO (Ptr b)
buildGCAction [] size = mallocBytesT size
buildGCAction stack size = do let rootsOnStack = concatMap T.possibleRefs stack
                              rootCandidates <- lift $ mapM dereference rootsOnStack
                              realRoots <- filterM (validRef . snd) rootCandidates
                              performCollection $ foldr buildRootPatcher M.empty realRoots
                              mallocBytesT size
  where --checkRef :: IntPtr -> StateT a IO Bool
        dereference :: IntPtr -> IO (IntPtr,IntPtr)
        dereference intPtr = do printfGc $ printf "deref stacklocation: 0x%08x\n" (fromIntegral intPtr :: Int)
                                obj <- peek $ intPtrToPtr intPtr :: IO IntPtr
                                printfGc $ printf "deref location: "
                                printfGc ((show $ intPtrToPtr obj) ++ "\n")
                                return (intPtr,obj)

-- (stackLocation,obj)
buildRootPatcher :: (IntPtr,IntPtr) -> RootSet a -> RootSet a
buildRootPatcher (ptr,obj) = M.insertWith (both) ptr' patch 
  where --patch = poke ptr' 
        patch newLocation = do printfGc $ printf "patch new ref: 0x%08x on stackloc: 0x%08x\n" 
                                 (fromIntegral newLocation :: Int) (fromIntegral ptr :: Int)
                               poke (intPtrToPtr ptr) newLocation   
        ptr' = intPtrToPtr obj

        both newPatch oldPatch = \newLocation -> do newPatch newLocation
                                                    oldPatch newLocation
