{-# LANGUAGE ExistentialQuantification #-}
module Mate.MemoryManager (evacuateList, AllocationManager(heapSize, performCollection), 
                           TwoSpace, initTwoSpace) where

import qualified Foreign.Marshal.Alloc as Alloc
import Foreign.Ptr
import Foreign.Marshal.Utils

import Text.Printf
import Control.Monad.State
import Control.Applicative

import Mate.GC

class AllocationManager a where
  
  -- | allocates n bytes in current space to space (may be to space or gen0 space)
  mallocBytes :: Int -> StateT a IO (Ptr b)
  
  -- | performs full gc and which is reflected in mem managers state
  performCollection :: (RefObj b) => [b] ->  StateT a IO ()

  heapSize :: StateT a IO Int

data TwoSpace = TwoSpace { fromBase :: IntPtr, 
                           toBase   :: IntPtr, 
                           fromHeap :: IntPtr, 
                           toHeap   :: IntPtr,
                           fromExtreme :: IntPtr,
                           toExtreme   :: IntPtr }

instance AllocationManager TwoSpace where
  mallocBytes = mallocBytes'
  performCollection = performCollection'
  
  heapSize = do space <- get
                return $ fromIntegral $ toHeap space - fromIntegral (toBase space)


performCollection' :: (RefObj a) => [a] -> StateT TwoSpace IO ()
performCollection' roots = do modify switchSpaces
                              newState <- get
                              lift (performCollectionIO newState roots)
                              -- [todo hs]: patch gc roots
                         
-- [todo hs] this is slow. merge phases to eliminate list with refs
performCollectionIO :: (AllocationManager b, RefObj a) => b -> [a] -> IO ()
performCollectionIO manager refs' = do lifeRefs <- liftM concat $ mapM (markTree'' marked mark []) refs'
                                       evacuateList lifeRefs manager
                                       patchAllRefs lifeRefs                       

switchSpaces :: TwoSpace -> TwoSpace
switchSpaces old = old { fromHeap = toHeap old,
                         toHeap = fromBase old, 
                         fromBase = toBase old,
                         toBase = fromBase old,
                         fromExtreme = toExtreme old,
                         toExtreme = fromExtreme old }


mallocBytes' :: Int -> StateT TwoSpace IO (Ptr b)
mallocBytes' bytes = do state' <- get
                        let end = toHeap state' + ptrToIntPtr (nullPtr `plusPtr` bytes) -- not really? FUUU
                        -- actually i would like to use an existential within TwoSpace but this requires
                        -- pattern matchingt at call site http://stackoverflow.com/questions/10192663/why-cant-existential-types-use-record-syntax which is i think even slower. 
                        if end <= toExtreme state' then alloc state' end else failNoSpace
  where alloc :: TwoSpace -> IntPtr -> StateT TwoSpace IO (Ptr b)
        alloc state' end = do let ptr = toHeap state'
                              put $ state' { toHeap = end } 
                              liftIO (putStrLn $ "Allocated obj: " ++ show ptr)
                              liftIO (return $ intPtrToPtr ptr)
        failNoSpace = error "no space left in two space (mallocBytes')"


evacuate' :: (RefObj a, AllocationManager b) => [a] -> StateT b IO ()
evacuate' =  mapM_ evacuate'' 

evacuate'' :: (RefObj a, AllocationManager b) => a -> StateT b IO ()
evacuate'' obj = do (size',payload') <- liftIO ((,) <$> size obj <*> payload obj)
                    -- malloc in TwoSpace
                    newPtr <- mallocBytes size'
                    liftIO (putStrLn ("evacuating: " ++ show obj ++ " and set: " ++ show newPtr))
                    -- copy data over and leave notice
                    liftIO (copyBytes newPtr (intPtrToPtr payload') size' >> 
                            setNewRef obj (cast newPtr))

evacuateList :: (RefObj a, AllocationManager b) => [a] -> b -> IO ()
evacuateList objs = evalStateT (evacuate' objs) 


initTwoSpace :: Int -> IO TwoSpace
initTwoSpace size' =  do printf "initializing TwoSpace memory manager with %d bytes.\n" size'
                         fromSpace <- Alloc.mallocBytes size'
                         toSpace   <- Alloc.mallocBytes size'
                         if fromSpace /= nullPtr && toSpace /= nullPtr 
                            then return $ buildToSpace fromSpace toSpace
                            else error "Could not initialize TwoSpace memory manager (malloc returned null ptr)\n"
   where buildToSpace from to = let fromBase' = ptrToIntPtr from
                                    toBase' = ptrToIntPtr to
                                    fromExtreme' = ptrToIntPtr $ from `plusPtr` size'
                                    toExtreme' = ptrToIntPtr $ to `plusPtr` size'
                                in TwoSpace { fromBase = fromBase', toBase = toBase',
                                              fromHeap = fromBase', toHeap = toBase',
                                              fromExtreme = fromExtreme', toExtreme = toExtreme' }

