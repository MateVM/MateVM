module MemoryManager ( ) where

import qualified Foreign.Marshal.Alloc as Alloc
import Foreign.Ptr
import Foreign.Storable

class AllocationManager a where
  mallocBytes :: a -> Int -> (a,Ptr b)

data TwoSpace = TwoSpace { basePtrA :: IntPtr, basePtrB :: IntPtr, heapPtrA :: IntPtr, heapPtrB :: IntPtr }

instance AllocationManager TwoSpace where
  mallocBytes = mallocBytes'

mallocBytes' :: TwoSpace -> Int -> (TwoSpace, Ptr a)
mallocBytes' = undefined
