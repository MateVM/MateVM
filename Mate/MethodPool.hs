{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Mate.MethodPool where

import Data.Binary
import Data.String
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B

import Text.Printf

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.StablePtr

import JVM.Converter

import Harpy
import Harpy.X86Disassembler

import Mate.BasicBlocks
import Mate.X86CodeGen


foreign import ccall "get_mmap"
  get_mmap :: IO (Ptr ())

foreign import ccall "set_mmap"
  set_mmap :: Ptr () -> IO ()


-- B.ByteString = name of method
-- Word32 = entrypoint of method
type MMap = M.Map B.ByteString Word32

-- TODO(bernhard): not in use yet
-- Word32 = point of method call
-- B.ByteString = name of called method
type CMap = M.Map Word32 B.ByteString

foreign export ccall getMethodEntry :: Ptr () -> CString -> IO CUInt
getMethodEntry :: Ptr () -> CString -> IO CUInt
getMethodEntry ptr_mmap cstr = do
  mmap <- ptr2mmap ptr_mmap
  str' <- peekCString cstr
  let method = fromString str'
  case M.lookup method mmap of
    Nothing -> do
      printf "getMethodEntry: no method found. compile it\n"
      -- TODO(bernhard): hardcoded... fixme!
      cls <- parseClassFile "tests/Fib.class"
      hmap <- parseMethod cls method
      case hmap of
        Just hmap' -> do
          entry <- compileBB hmap' method
          let w32_entry = ((fromIntegral $ ptrToIntPtr entry) :: Word32)
          let mmap' = M.insert method w32_entry mmap
          mmap2ptr mmap' >>= set_mmap
          return $ fromIntegral w32_entry
        Nothing -> error $ (show method) ++ " not found. abort"
    Just w32 -> return (fromIntegral w32)

-- t_01 :: IO ()
-- t_01 = do
--   (entry, _) <- testCase "./tests/Fib.class" "fib"
--   let int_entry = ((fromIntegral $ ptrToIntPtr entry) :: Word32)
--   let mmap = M.insert ("fib" :: String) int_entry M.empty
--   mapM_ (\(x,y) -> printf "%s at 0x%08x\n" x y) $ M.toList mmap
--   mmap2ptr mmap >>= set_mmap
--   demo_mmap -- access Data.Map from C

initMethodPool :: IO ()
initMethodPool = mmap2ptr M.empty >>= set_mmap

compileBB :: MapBB -> B.ByteString -> IO (Ptr Word8)
compileBB hmap name = do
  mmap <- get_mmap >>= ptr2mmap
  let ebb = emitFromBB hmap
  (_, Right ((entry, bbstarts, end), disasm)) <- runCodeGen ebb () ()
  let w32_entry = ((fromIntegral $ ptrToIntPtr entry) :: Word32)
  let mmap' = M.insert name w32_entry mmap
  mmap2ptr mmap' >>= set_mmap
  printf "disasm:\n"
  mapM_ (putStrLn . showAtt) disasm
  return entry

foreign import ccall "dynamic"
   code_void :: FunPtr (IO ()) -> (IO ())

executeFuncPtr :: Ptr Word8 -> IO ()
executeFuncPtr entry = code_void $ ((castPtrToFunPtr entry) :: FunPtr (IO ()))

mmap2ptr :: MMap -> IO (Ptr ())
mmap2ptr mmap = do
  ptr_mmap <- newStablePtr mmap
  return $ castStablePtrToPtr ptr_mmap

ptr2mmap :: Ptr () -> IO MMap
ptr2mmap vmap = deRefStablePtr $ ((castPtrToStablePtr vmap) :: StablePtr MMap)
