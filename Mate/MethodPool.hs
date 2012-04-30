{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Mate.MethodPool where

import Data.Binary
import Data.String.Utils
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B
import System.Plugins

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import JVM.ClassFile

import Harpy
#ifdef DEBUG
import Harpy.X86Disassembler

import Text.Printf
#endif

import Mate.BasicBlocks
import Mate.Types
import Mate.X86CodeGen
import Mate.Utilities
import Mate.ClassPool


foreign import ccall "dynamic"
   code_void :: FunPtr (IO ()) -> (IO ())

foreign export ccall getTrapType :: CUInt -> CUInt -> IO CUInt
getTrapType :: CUInt -> CUInt -> IO CUInt
getTrapType signal_from from2 = do
  tmap <- get_trapmap >>= ptr2trapmap
  case M.lookup (fromIntegral signal_from) tmap of
    (Just (MI _)) -> return 0
    (Just (VI _)) -> return 1
    (Just (SFI _)) -> return 2
    (Just (II _)) -> return 4
    -- maybe we've a hit on the second `from' value
    Nothing -> case M.lookup (fromIntegral from2) tmap of
      (Just (VI _)) -> return 1
      (Just (II _)) -> return 4
      (Just _) -> error $ "getTrapType: abort #1 :-("
      Nothing -> error $ "getTrapType: abort #2 :-("

foreign export ccall getMethodEntry :: CUInt -> CUInt -> IO CUInt
getMethodEntry :: CUInt -> CUInt -> IO CUInt
getMethodEntry signal_from methodtable = do
  mmap <- get_methodmap >>= ptr2methodmap
  tmap <- get_trapmap >>= ptr2trapmap
  vmap <- get_virtualmap >>= ptr2virtualmap

  let w32_from = fromIntegral signal_from
  let mi = tmap M.! w32_from
  let mi'@(MethodInfo method cm sig) =
        case mi of
          (MI x) -> x
          (VI (MethodInfo methname _ msig)) ->
              (MethodInfo methname (vmap M.! (fromIntegral methodtable)) msig)
          (II (MethodInfo methname _ msig)) ->
              (MethodInfo methname (vmap M.! (fromIntegral methodtable)) msig)
          _ -> error $ "getMethodEntry: no trapInfo. abort."
  case M.lookup mi' mmap of
    Nothing -> do
      cls <- getClassFile cm
#ifdef DEBUG
      printf "getMethodEntry(from 0x%08x): no method \"%s\" found. compile it\n" w32_from (show mi')
#endif
      mm <- lookupMethodRecursive method [] cls
      case mm of
        Just (mm', clsnames, cls') -> do
            let flags = methodAccessFlags mm'
            case S.member ACC_NATIVE flags of
              False -> do
                hmap <- parseMethod cls' method
                case hmap of
                  Just hmap' -> do
                    entry <- compileBB hmap' (MethodInfo method (thisClass cls') sig)
                    addMethodRef entry mi' clsnames
                    return $ fromIntegral entry
                  Nothing -> error $ (show method) ++ " not found. abort"
              True -> do
                -- TODO(bernhard): cleaner please... *do'h*
                let symbol = (replace "/" "_" $ toString cm) ++ "__" ++ (toString method) ++ "__" ++ (replace ";" "_" $ replace "/" "_" $ replace "(" "_" (replace ")" "_" $ toString $ encode sig))
#ifdef DEBUG
                printf "native-call: symbol: %s\n" symbol
#endif
                nf <- loadNativeFunction symbol
                let w32_nf = fromIntegral nf
                let mmap' = M.insert mi' w32_nf mmap
                methodmap2ptr mmap' >>= set_methodmap
                return nf
        Nothing -> error $ (show method) ++ " not found. abort"
    Just w32 -> return (fromIntegral w32)

lookupMethodRecursive :: B.ByteString -> [B.ByteString] -> Class Resolved
                         -> IO (Maybe ((Method Resolved, [B.ByteString], Class Resolved)))
lookupMethodRecursive name clsnames cls = do
  case res of
    Just x -> return $ Just (x, nextclsn, cls)
    Nothing -> if thisname == "java/lang/Object"
      then return $ Nothing
      else do
        supercl <- getClassFile (superClass cls)
        lookupMethodRecursive name nextclsn supercl
  where
  res = lookupMethod name cls
  thisname = thisClass cls
  nextclsn :: [B.ByteString]
  nextclsn = thisname:clsnames

-- TODO(bernhard): UBERHAX.  ghc patch?
foreign import ccall safe "lookupSymbol"
   c_lookupSymbol :: CString -> IO (Ptr a)

loadNativeFunction :: String -> IO (CUInt)
loadNativeFunction sym = do
        _ <- loadRawObject "ffi/native.o"
        -- TODO(bernhard): WTF
        resolveObjs (return ())
        ptr <- withCString sym c_lookupSymbol
        if (ptr == nullPtr)
          then error $ "dyn. loading of \"" ++ sym ++ "\" failed."
          else return $ fromIntegral $ ptrToIntPtr ptr

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
  methodmap2ptr M.empty >>= set_methodmap
  trapmap2ptr M.empty >>= set_trapmap
  classmap2ptr M.empty >>= set_classmap
  virtualmap2ptr M.empty >>= set_virtualmap
  stringsmap2ptr M.empty >>= set_stringsmap
  interfacesmap2ptr M.empty >>= set_interfacesmap
  interfacemethodmap2ptr M.empty >>= set_interfacemethodmap


addMethodRef :: Word32 -> MethodInfo -> [B.ByteString] -> IO ()
addMethodRef entry (MethodInfo mmname _ msig) clsnames = do
  mmap <- get_methodmap >>= ptr2methodmap
  let newmap = M.fromList $ map (\x -> ((MethodInfo mmname x msig), entry)) clsnames
  methodmap2ptr (mmap `M.union` newmap) >>= set_methodmap


compileBB :: MapBB -> MethodInfo -> IO Word32
compileBB hmap methodinfo = do
  tmap <- get_trapmap >>= ptr2trapmap

  cls <- getClassFile (methClassName methodinfo)
  let ebb = emitFromBB (methName methodinfo) cls hmap
  (_, Right right) <- runCodeGen ebb () ()

  let ((entry, _, _, new_tmap), _) = right
  let tmap' = M.union tmap new_tmap -- prefers elements in cmap
  trapmap2ptr tmap' >>= set_trapmap

#ifdef DEBUG
  printf "disasm:\n"
  mapM_ (putStrLn . showAtt) (snd right)
#endif
  -- UNCOMMENT NEXT LINE FOR GDB FUN
  -- _ <- getLine
  -- (1) start it with `gdb ./mate' and then `run <classfile>'
  -- (2) on getLine, press ctrl+c
  -- (3) `br *0x<addr>'; obtain the address from the disasm above
  -- (4) `cont' and press enter
  return $ fromIntegral $ ptrToIntPtr entry


executeFuncPtr :: Word32 -> IO ()
executeFuncPtr entry =
  code_void $ ((castPtrToFunPtr $ intPtrToPtr $ fromIntegral entry) :: FunPtr (IO ()))
