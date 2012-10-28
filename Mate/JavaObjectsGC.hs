module JavaObjectsGC where

import Mate.GC
import Mate.MemoryManager

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import GHC.Int
import Text.Printf
import System.IO.Unsafe(unsafePerformIO)
import Data.IORef
import qualified Data.Map as M

import Control.Monad
import Control.Monad.State

import Test.QuickCheck 
import Test.QuickCheck.Monadic 

instance RefObj (Ptr a) where
  payload     = return . ptrToIntPtr
  size a      = fmap ((+ fieldsOff) . (*4) . length) (refs a)
  refs        = unpackRefs . castPtr
  marked      = markedRef
  mark        = markRef (0x1::Int32)
  unmark      = markRef (0x0::Int32)
  setNewRef   = setNewRefPtr
  patchRefs   = patchRefsPtr
  cast = castPtr
  getNewRef ptr = peekByteOff ptr newRefOff

instance PrintableRef (Ptr a) where
  printRef    = printRef'


