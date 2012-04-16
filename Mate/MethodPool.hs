{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Mate.MethodPool where

import Data.Binary
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B

import Text.Printf

import Foreign.Ptr
import Foreign.C.Types
import Foreign.StablePtr

import JVM.ClassFile

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

foreign export ccall getMethodEntry :: CUInt -> Ptr () -> Ptr () -> IO CUInt
getMethodEntry :: CUInt -> Ptr () -> Ptr () -> IO CUInt
getMethodEntry signal_from ptr_mmap ptr_cmap = do
  mmap <- ptr2mmap ptr_mmap
  cmap <- ptr2cmap ptr_cmap

  let w32_from = fromIntegral signal_from
  let (method, cls, cpidx) = cmap M.! w32_from
  case M.lookup method mmap of
    Nothing -> do
      printf "getMethodEntry(from 0x%08x): no method found. compile it\n" w32_from
      -- TODO(bernhard): maybe we have to load the class first?
      --                 (Or better in X86CodeGen?)
      let (CMethod _ nt) = (constsPool cls) M.! cpidx
      hmap <- parseMethod cls (ntName nt)
      case hmap of
        Just hmap' -> do
          entry <- compileBB hmap' cls method
          return $ fromIntegral $ ((fromIntegral $ ptrToIntPtr entry) :: Word32)
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
initMethodPool = do
  mmap2ptr M.empty >>= set_mmap
  cmap2ptr M.empty >>= set_cmap

compileBB :: MapBB -> Class Resolved -> B.ByteString -> IO (Ptr Word8)
compileBB hmap cls name = do
  mmap <- get_mmap >>= ptr2mmap
  cmap <- get_cmap >>= ptr2cmap

  let ebb = emitFromBB cls hmap
  (_, Right ((entry, _, _, new_cmap), disasm)) <- runCodeGen ebb () ()
  let w32_entry = ((fromIntegral $ ptrToIntPtr entry) :: Word32)

  let mmap' = M.insert name w32_entry mmap
  let cmap' = M.union cmap new_cmap -- prefers elements in cmap
  mmap2ptr mmap' >>= set_mmap
  cmap2ptr cmap' >>= set_cmap

  printf "disasm:\n"
  mapM_ (putStrLn . showAtt) disasm
  -- UNCOMMENT NEXT LINE FOR GDB FUN
  -- _ <- getLine
  -- (1) start it with `gdb ./mate' and then `run <classfile>'
  -- (2) on getLine, press ctrl+c
  -- (3) `br *0x<addr>'; obtain the address from the disasm above
  -- (4) `cont' and press enter
  return entry


foreign import ccall "dynamic"
   code_void :: FunPtr (IO ()) -> (IO ())

executeFuncPtr :: Ptr Word8 -> IO ()
executeFuncPtr entry = code_void $ ((castPtrToFunPtr entry) :: FunPtr (IO ()))

-- TODO(bernhard): make some typeclass magic 'n stuff
mmap2ptr :: MMap -> IO (Ptr ())
mmap2ptr mmap = do
  ptr_mmap <- newStablePtr mmap
  return $ castStablePtrToPtr ptr_mmap

ptr2mmap :: Ptr () -> IO MMap
ptr2mmap vmap = deRefStablePtr $ ((castPtrToStablePtr vmap) :: StablePtr MMap)

cmap2ptr :: CMap -> IO (Ptr ())
cmap2ptr cmap = do
  ptr_cmap <- newStablePtr cmap
  return $ castStablePtrToPtr ptr_cmap

ptr2cmap :: Ptr () -> IO CMap
ptr2cmap vmap = deRefStablePtr $ ((castPtrToStablePtr vmap) :: StablePtr cmap)
