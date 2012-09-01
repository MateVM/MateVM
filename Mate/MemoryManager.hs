{-# LANGUAGE ExistentialQuantification #-}
module Mate.MemoryManager (evacuateList, AllocationManager, 
                           TwoSpace, initTwoSpace) where

import qualified Foreign.Marshal.Alloc as Alloc
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Utils

import Text.Printf
import Control.Monad.State
import Control.Applicative

import Mate.GC

class AllocationManager a where
  mallocBytes :: Int -> StateT a IO (Ptr b)

data TwoSpace = TwoSpace { fromBase :: IntPtr, 
                           toBase   :: IntPtr, 
                           fromHeap :: IntPtr, 
                           toHeap   :: IntPtr,
                           fromExtreme :: IntPtr,
                           toExtreme   :: IntPtr }

instance AllocationManager TwoSpace where
  mallocBytes = mallocBytes'

mallocBytes' :: Int -> StateT TwoSpace IO (Ptr b)
mallocBytes' bytes = do state <- get
                        let end = toHeap state + ptrToIntPtr (nullPtr `plusPtr` bytes) -- not really? FUUU
                        -- actually i would like to use an existential within TwoSpace but this requires
                        -- pattern matchingt at call site http://stackoverflow.com/questions/10192663/why-cant-existential-types-use-record-syntax which is i think even slower. 
                        if end <= toExtreme state then alloc state end else fail
  where alloc :: TwoSpace -> IntPtr -> StateT TwoSpace IO (Ptr b)
        alloc state end = do let ptr = toHeap state
                             put $ state { toHeap = end } 
                             liftIO (putStrLn $ "Allocated obj: " ++ (show ptr))
                             liftIO (return $ intPtrToPtr ptr)
        fail = error "no space left in two space (mallocBytes')"


evacuate' :: (RefObj a, AllocationManager b) => [a] -> StateT b IO ()
evacuate' = foldr (\x evac -> evac >> evacuate'' x) (liftIO (return ())) 

evacuate'' :: (RefObj a, AllocationManager b) => a -> StateT b IO ()
evacuate'' obj = do (size,payload) <- liftIO ((,) <$> size obj <*> payload obj)
                    -- malloc in TwoSpace
                    newPtr <- mallocBytes size
                    liftIO (putStrLn ("evacuating: " ++ show obj ++ " and set: " ++ show newPtr))
                    -- copy data over and leave notice
                    liftIO (copyBytes newPtr (intPtrToPtr payload) size >> 
                            newRef obj (cast newPtr))

evacuateList :: (RefObj a, AllocationManager b) => [a] -> b -> IO ()
evacuateList objs manager = evalStateT (evacuate' objs) manager


initTwoSpace :: Int -> IO TwoSpace
initTwoSpace size =  do printf "initializing TwoSpace memory manager with %d bytes.\n" size
                        fromSpace <- Alloc.mallocBytes size
                        toSpace   <- Alloc.mallocBytes size
                        if fromSpace /= nullPtr && toSpace /= nullPtr 
                           then return $ buildToSpace fromSpace toSpace
                           else error "Could not initialize TwoSpace memory manager (malloc returned null ptr)\n"
   where buildToSpace from to = let fromBase' = ptrToIntPtr from
                                    toBase' = ptrToIntPtr to
                                    fromExtreme' = ptrToIntPtr $ from `plusPtr` size
                                    toExtreme' = ptrToIntPtr $ to `plusPtr` size
                                in TwoSpace { fromBase = fromBase', toBase = toBase',
                                              fromHeap = fromBase', toHeap = toBase',
                                              fromExtreme = fromExtreme', toExtreme = toExtreme' }

