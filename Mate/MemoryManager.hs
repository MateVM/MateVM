module MemoryManager ( ) where

import qualified Foreign.Marshal.Alloc as Alloc
import Foreign.Ptr
import Foreign.Storable

import Control.Monad.State

--import GC

class AllocationManager a where
  mallocBytes :: a -> Int -> (a,Ptr b)

data TwoSpace = TwoSpace { fromBase :: IntPtr, 
                           toBase   :: IntPtr, 
                           fromHeap :: IntPtr, 
                           toHeap   :: IntPtr,
                           fromExtreme :: IntPtr,
                           toExtreme   :: IntPtr }

mallocBytes' :: Int -> State TwoSpace (Ptr b)
mallocBytes' bytes = do state <- get
                        let end = (toHeap state) + (ptrToIntPtr $ nullPtr `plusPtr` bytes) -- not really? FUUU
                        -- actually i would like to use an existential within TwoSpace but this requires
                        -- pattern matchingt at call site http://stackoverflow.com/questions/10192663/why-cant-existential-types-use-record-syntax which is i think even slower. 
                        if end <= toExtreme state then alloc state end else fail
  where alloc :: TwoSpace -> IntPtr -> State TwoSpace (Ptr b)
        alloc state end = do let ptr = toHeap state
                             put $ state { toHeap = end } 
                             return $ intPtrToPtr ptr
        fail = error "no space left in two space (mallocBytes')"
