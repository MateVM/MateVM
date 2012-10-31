{-# LANGUAGE ExistentialQuantification #-}
module Mate.MemoryManager   
    ( AllocationManager(..)
    , TwoSpace(..)
    , initTwoSpace
    , mallocBytes'
    , RefUpdateAction
    , buildGCAction )   where

import qualified Foreign.Marshal.Alloc as Alloc
import Foreign.Ptr
import Foreign.Marshal.Utils
import Foreign.Storable
import GHC.Int

import Text.Printf
import Control.Monad.State
import Control.Applicative
import qualified Data.Map as M
import Data.List

import Mate.Debug
import Mate.GC hiding (size)
import qualified Mate.StackTrace as T
import Mate.JavaObjectsGC() -- only instances for Ptr a
import qualified Mate.GC as GC

type RootSet a = M.Map (Ptr a) RefUpdateAction


data TwoSpace = TwoSpace { fromBase :: IntPtr, 
                           toBase   :: IntPtr, 
                           fromHeap :: IntPtr, 
                           toHeap   :: IntPtr,
                           fromExtreme :: IntPtr,
                           toExtreme   :: IntPtr,
                           validRange :: (IntPtr,IntPtr) }

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
                              liftIO (printfGc "rootSet: " >> printGc rootList)
                              performCollectionIO rootList
                              liftIO $ patchGCRoots roots

patchGCRoots :: (RefObj a) => M.Map a RefUpdateAction -> IO ()
patchGCRoots roots = mapM_ fixRef $ M.toList roots
  where fixRef (obj,fixupAction) = getNewRef obj >>= getIntPtr >>= fixupAction


markedOrInvalid :: (RefObj a) => StateT TwoSpace IO (a -> IO Bool)
markedOrInvalid = do memManager <- get
                     return $ \obj -> do objAsPtr <- getIntPtr obj
                                         let valid = validRef' objAsPtr memManager
                                         if valid 
                                          then liftM not (marked obj)
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
                                printfGc (show $ intPtrToPtr obj)
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

switchSpaces :: TwoSpace -> TwoSpace
switchSpaces old = old { fromHeap = toHeap old,
                         toHeap = fromHeap old, 
                         fromBase = toBase old,
                         toBase = fromBase old,
                         fromExtreme = toExtreme old,
                         toExtreme = fromExtreme old }


mallocBytes' :: Int -> StateT TwoSpace IO (Ptr b)
mallocBytes' bytes = do state' <- get
                        let end = toHeap state' + fromIntegral bytes
                            
                            base = fromIntegral $ toBase state'
                            extreme = fromIntegral $ toExtreme state'
                            heap = fromIntegral $ toHeap state'
                            used = heap - base
                            capacity = extreme - base
                        if end <= toExtreme state' 
                          then liftIO (logAllocation bytes used capacity) >> alloc state' end 
                          else 
                               failNoSpace used capacity
  where alloc :: TwoSpace -> IntPtr -> StateT TwoSpace IO (Ptr b)
        alloc state' end = do let ptr = toHeap state'
                              put $ state' { toHeap = end  } 
                              liftIO (printfGc $ "Allocated obj: " ++ show (intPtrToPtr ptr) ++ "\n")
                              liftIO (return $ intPtrToPtr ptr)
        failNoSpace :: Integer -> Integer -> a
        failNoSpace usage fullSize = 
            error $ printf "no space left in two space (mallocBytes'). Usage: %d/%d" usage fullSize
        
        logAllocation :: Int -> Integer -> Integer -> IO ()
        --logAllocation _ _ _ = return ()
        logAllocation fullSize usage capacity = printfGc $ printf "alloc size: %d (%d/%d)\n" fullSize usage capacity
                          


evacuate' :: (RefObj a, AllocationManager b) => [a] -> StateT b IO ()
evacuate' =  mapM_ evacuate'' 

evacuate'' :: (RefObj a, AllocationManager b) => a -> StateT b IO ()
evacuate'' obj = do (size,location) <- liftIO ((,) <$> getSizeDebug obj <*> getIntPtr obj)
                    -- malloc in TwoSpace
                    newPtr <- mallocBytesT size
                    liftIO (printfGc ("evacuating: " ++ show obj ++ 
                                         " and set: " ++ show newPtr ++ " size: " ++ show size ++ "\n"))
                    -- copy data over and leave notice
                    liftIO (copyBytes newPtr (intPtrToPtr location) size >> 
                            setNewRef obj (cast newPtr) >>
                            pokeByteOff newPtr 4 (0::Int32))

getSizeDebug :: RefObj a => a -> IO Int
getSizeDebug obj = do 
  intObj <- getIntPtr obj
  printfGc $ printf "objTo evacuate: 0x%08x\n" (fromIntegral intObj :: Int)
  size <- GC.size obj
  printfGc $ printf "size was %i\n" size
  return size

--evacuateList :: (RefObj a, AllocationManager b) => [a] -> b -> StateT b IO ()
--evacuateList objs = evacuate' objs

validRef' :: IntPtr -> TwoSpace -> Bool
validRef' ptr twoSpace = (ptr >= (fst $ validRange twoSpace)) && 
                         (ptr <= (snd $ validRange twoSpace))

initTwoSpace :: Int -> IO TwoSpace
initTwoSpace size' =  do printfStr $ printf "initializing TwoSpace memory manager with %d bytes.\n" size'
                         fromSpace <- Alloc.mallocBytes (size' * 2)
                         let toSpace   = fromSpace `plusPtr` size'
                         if fromSpace /= nullPtr && toSpace /= nullPtr 
                            then return $ buildToSpace fromSpace toSpace
                            else error "Could not initialize TwoSpace memory manager (malloc returned null ptr)\n"
   where buildToSpace from to = let fromBase' = ptrToIntPtr from
                                    toBase' = ptrToIntPtr to
                                    fromExtreme' = ptrToIntPtr $ from `plusPtr` size'
                                    toExtreme' = ptrToIntPtr $ to `plusPtr` size'
                                in TwoSpace { fromBase = fromBase', toBase = toBase',
                                              fromHeap = fromBase', toHeap = toBase',
                                              fromExtreme = fromExtreme', toExtreme = toExtreme',
                                              validRange = (fromBase',toExtreme') }

