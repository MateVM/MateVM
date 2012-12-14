module Compiler.Mate.Runtime.MemoryManager   
    ( AllocationManager(..)
    , buildGCAction
    , dereference
    , RootSet
    , buildRootPatcher
    , extractLargeObjects
    , markedOrInvalid
    , hasMTable)   where

import Foreign

import Text.Printf
import Control.Monad.State
import qualified Data.Map as M
import Data.List

import Compiler.Mate.Flags
import Compiler.Mate.Debug
import Compiler.Mate.Runtime.GC hiding (size)
import qualified Compiler.Mate.Runtime.StackTrace as T
import qualified Compiler.Mate.Runtime.JavaObjectsGC as GCObj
import Compiler.Mate.Runtime.JavaObjectsGC(hasMTable) -- only instances for Ptr a

type RootSet a = M.Map (Ptr a) RefUpdateAction


markedOrInvalid :: (RefObj a, AllocationManager b) => StateT b IO (a -> IO Bool)
markedOrInvalid = 
  return $ \obj -> do objAsPtr <- getIntPtr obj
                      printfGc $ printf "check obj: 0x%08x" (fromIntegral objAsPtr :: Int)
                      --let valid = validRef' objAsPtr memManager
                      if hasMTable objAsPtr-- this was not necassary before perm gens (now direct refs onto objs) 
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


buildGCAction :: AllocationManager a => GenInfo -> [T.StackDescription] -> [IntPtr] -> Int -> StateT a IO (Ptr b)
buildGCAction info [] _ size = mallocBytesT info size
buildGCAction info stack perm size = 
    do let rootsOnStack = perm ++ concatMap T.candidates stack --concatMap T.possibleRefs stack
       rootCandidates <- lift $ mapM dereference rootsOnStack
       realRoots <- filterM (notNullRef . snd) rootCandidates
       performCollection $ foldr buildRootPatcher M.empty realRoots
       mallocBytesT info size
       

dereference :: IntPtr -> IO (IntPtr,IntPtr)
dereference intPtr = do 
    printfGc $ printf "rootReference (stacklocation): 0x%08x\n" (fromIntegral intPtr :: Int)
    obj <- peek $ intPtrToPtr intPtr :: IO IntPtr
    printfGc $ printf "*(rootElement): "
    printfGc (show (intPtrToPtr obj) ++ "\n")
    return (intPtr,obj)

-- (stackLocation,obj)
buildRootPatcher :: (IntPtr,IntPtr) -> RootSet a -> RootSet a
buildRootPatcher (ptr,obj) = M.insertWith both ptr' patch 
  where patch newLocation = do printfGc $ printf "patch new ref: 0x%08x on stackloc: 0x%08x .. " 
                                 (fromIntegral newLocation :: Int) (fromIntegral ptr :: Int)
                               poke (intPtrToPtr ptr) newLocation  
                               printfPlain "=>patched.\n"
        ptr' = intPtrToPtr obj

        both newPatch oldPatch newLocation = do newPatch newLocation
                                                oldPatch newLocation
