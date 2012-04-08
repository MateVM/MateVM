{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Mate.MethodPool where

import Data.Binary
import qualified Data.Map as M


import Text.Printf

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.StablePtr

import Mate.X86CodeGen


foreign import ccall "get_mmap"
  get_mmap :: IO (Ptr ())

foreign import ccall "set_mmap"
  set_mmap :: Ptr () -> IO ()

foreign import ccall "demo_mmap"
  demo_mmap :: IO ()


type MMap = M.Map String Word32

foreign export ccall getMethodEntry :: Ptr () -> CString -> IO CUInt
getMethodEntry :: Ptr () -> CString -> IO CUInt
getMethodEntry ptr_mmap cstr = do
  mmap <- deRefStablePtr $ ((castPtrToStablePtr ptr_mmap) :: StablePtr MMap)
  k <- peekCString cstr
  case M.lookup k mmap of
    Nothing -> return 0
    Just w32 -> return (fromIntegral w32)

t_01 :: IO ()
t_01 = do
  (entry, _) <- testCase "./tests/Fib.class" "fib"
  let int_entry = ((fromIntegral $ ptrToIntPtr entry) :: Word32)
  let mmap = M.insert ("fib" :: String) int_entry M.empty
  mapM_ (\(x,y) -> printf "%s at 0x%08x\n" x y) $ M.toList mmap
  ptr_mmap <- newStablePtr mmap
  set_mmap $ castStablePtrToPtr ptr_mmap
  demo_mmap -- access Data.Map from C
