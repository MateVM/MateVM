module Compiler.Mate.Runtime.TwoSpaceAllocator where

import Foreign
import Control.Monad.State
import qualified Foreign.Marshal.Alloc as Alloc
import Data.Set (Set) 
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List

import Compiler.Mate.Runtime.MemoryManager
import Compiler.Mate.Runtime.RtsOptions
import Compiler.Mate.Runtime.GC hiding (size)
import Compiler.Mate.Debug

data TwoSpace = TwoSpace { fromBase :: IntPtr, 
                           toBase   :: IntPtr, 
                           fromHeap :: IntPtr, 
                           toHeap   :: IntPtr,
                           fromExtreme :: IntPtr,
                           toExtreme   :: IntPtr,
                           validRange :: (IntPtr,IntPtr),
                           loh :: Set IntPtr
                         }


instance AllocationManager TwoSpace where
  initMemoryManager = initTwoSpace
  mallocBytesT _ = mallocBytes'
  performCollection = performCollection'

  collectLoh = collectLohTwoSpace
  
  heapSize = do space <- get
                return $ fromIntegral $ toHeap space - fromIntegral (toBase space)

  validRef _  = return True --liftM (validRef' ptr) get


switchSpaces :: TwoSpace -> TwoSpace
switchSpaces old = old { fromHeap = toHeap old,
                         toHeap = fromHeap old, 
                         fromBase = toBase old,
                         toBase = fromBase old,
                         fromExtreme = toExtreme old,
                         toExtreme = fromExtreme old }


mallocBytes' :: Int -> StateT TwoSpace IO (Ptr b)
mallocBytes' bytes = 
      do state' <- get
         if bytes < loThreshhold || not useLoh 
           then do
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
            else 
              allocateLoh bytes
  where alloc :: TwoSpace -> IntPtr -> StateT TwoSpace IO (Ptr b)
        alloc state' end = do 
                              let ptr = toHeap state'
                              put $ state' { toHeap = end  } 
                              liftIO (printfGc $ "Allocated obj: " ++ show (intPtrToPtr ptr) ++ "\n")
                              liftIO (return $ intPtrToPtr ptr)
        failNoSpace :: Integer -> Integer -> a
        failNoSpace usage fullSize = 
            error $ printf "no space left in two space (mallocBytes'). Usage: %d/%d" usage fullSize
        
        logAllocation :: Int -> Integer -> Integer -> IO ()
        --logAllocation _ _ _ = return ()
        logAllocation fullSize usage capacity = printfGc $ printf "alloc size: %d (%d/%d)\n" fullSize usage capacity
                          
allocateLoh :: Int -> StateT TwoSpace IO (Ptr b)
allocateLoh size = do
    current <- get
    let currentLoh = loh current
    ptr <- liftIO $ Alloc.mallocBytes size
    put $ current { loh = S.insert (ptrToIntPtr ptr) currentLoh }
    liftIO $ printfGc $ printf "LOH: allocated %d bytes in loh %s" size (show ptr)
    return ptr


--evacuateList :: (RefObj a, AllocationManager b) => [a] -> b -> StateT b IO ()
--evacuateList objs = evacuate' objs

validRef' :: IntPtr -> TwoSpace -> Bool
validRef' ptr twoSpace = (ptr >= fst (validRange twoSpace)) && 
                         (ptr <= snd (validRange twoSpace))

collectLohTwoSpace :: (RefObj a) => [a] -> StateT TwoSpace IO ()
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



initTwoSpace :: Int -> IO TwoSpace
initTwoSpace size' =  do printfStr $ printf "initializing TwoSpace memory manager with %d bytes.\n" size'
                         fromSpace <- Alloc.mallocBytes (size' * 2)
                         printfMem $ printf "memory area by gc: 0x%08x to 0x%08x\n" ((fromIntegral $ ptrToIntPtr fromSpace)::Word32) (size'*2 + fromIntegral (ptrToIntPtr fromSpace))
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
                                              validRange = (fromBase',toExtreme'),
                                              loh = S.empty}


performCollection' :: (RefObj a) => M.Map a RefUpdateAction -> StateT TwoSpace IO ()
performCollection' roots = do modify switchSpaces
                              let rootList = map fst $ M.toList roots
                              logGcT $ printf  "rootSet: %s\n " (show rootList)
                              performCollectionIO rootList
                              liftIO $ patchGCRoots roots
                              logGcT "all done \\o/"


-- [todo hs] this is slow. merge phases to eliminate list with refs
performCollectionIO :: (RefObj a, AllocationManager b) => [a] -> StateT b IO ()
performCollectionIO refs' = do 
  logGcT "==>Phase 1. Marking..\n"
  objFilter <- markedOrInvalid
  allLifeRefs <- liftIO $ liftM (nub . concat) $ mapM (markTree'' objFilter mark refs') refs'
  logGcT "==>Done Phase 1.\n"
  toEvacuate <- liftIO $ filterM (getIntPtr >=> return . hasMTable) allLifeRefs 
  if gcLogEnabled 
    then  liftIO $ mapM_ (getIntPtr >=> \x -> printfGc $ printf " 0x%08x" (fromIntegral x ::Int) ) toEvacuate
    else return ()
  (largeObjs,lifeRefs) <- liftIO $ extractLargeObjects toEvacuate
  logGcT "\nPhase 2. Evacuating...\n"

  if gcLogEnabled 
    then  liftIO $ mapM_ (getIntPtr >=> \x -> printfGc $ printf " 0x%08x" (fromIntegral x ::Int) ) lifeRefs
    else return ()
  evacuate' (const True) (\_ -> return mkGen0) lifeRefs
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
