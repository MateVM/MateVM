{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#include "debug.h"
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
import Harpy.X86Disassembler

#ifdef DEBUG
import Text.Printf
#endif

import Mate.BasicBlocks
import Mate.Types
import Mate.NativeMaschine
import Mate.ClassPool
import Mate.Debug
import Mate.Utilities

foreign import ccall "dynamic"
   code_void :: FunPtr (IO ()) -> IO ()


getMethodEntry :: CUInt -> CUInt -> IO CUInt
getMethodEntry signal_from methodtable = do
  mmap <- getMethodMap
  tmap <- getTrapMap
  vmap <- getVirtualMap

  let w32_from = fromIntegral signal_from
  let mi = tmap M.! w32_from
  let mi'@(MethodInfo method cm sig) =
       case mi of
         (StaticMethod x) -> x
         (VirtualMethod   _ (MethodInfo methname _ msig)) -> newMi methname msig
         (InterfaceMethod _ (MethodInfo methname _ msig)) -> newMi methname msig
         _ -> error "getMethodEntry: no TrapCause found. abort."
       where newMi mn = MethodInfo mn (vmap M.! fromIntegral methodtable)
  -- bernhard (TODO): doesn't work with gnu classpath at some point. didn't
  --                  figured out the problem yet :/ therefore, I have no
  --                  testcase for replaying the situation.
  -- setTrapMap $ M.delete w32_from tmap
  entryaddr <- case M.lookup mi' mmap of
    Nothing -> do
      cls <- getClassFile cm
      printfMp "getMethodEntry(from 0x%08x): no method \"%s\" found. compile it\n" w32_from (show mi')
      mm <- lookupMethodRecursive method sig [] cls
      case mm of
        Just (mm', clsnames, cls') -> do
            let flags = methodAccessFlags mm'
            if S.member ACC_NATIVE flags
              then do
                -- TODO(bernhard): cleaner please... *do'h*
                let sym1 = replace "/" "_" $ toString cm
                    parenth = replace "(" "_" $ replace ")" "_" $ toString $ encode sig
                    sym2 = replace ";" "_" $ replace "/" "_" parenth
                    symbol = sym1 ++ "__" ++ toString method ++ "__" ++ sym2
                printfMp "native-call: symbol: %s\n" symbol
                nf <- loadNativeFunction symbol
                setMethodMap $ M.insert mi' nf mmap
                return nf
              else do
                rawmethod <- parseMethod cls' method sig
                entry <- compileBB rawmethod (MethodInfo method (thisClass cls') sig)
                addMethodRef entry mi' clsnames
                return $ fromIntegral entry
        Nothing -> error $ show method ++ " not found. abort"
    Just w32 -> return w32
  return $ fromIntegral entryaddr

lookupMethodRecursive :: B.ByteString -> MethodSignature -> [B.ByteString] -> Class Direct
                         -> IO (Maybe (Method Direct, [B.ByteString], Class Direct))
lookupMethodRecursive name sig clsnames cls =
  case res of
    Just x -> return $ Just (x, nextclsn, cls)
    Nothing -> if thisname == "java/lang/Object"
      then return Nothing
      else do
        supercl <- getClassFile (superClass cls)
        lookupMethodRecursive name sig nextclsn supercl
  where
    res = lookupMethodSig name sig cls
    thisname = thisClass cls
    nextclsn :: [B.ByteString]
    nextclsn = thisname:clsnames

-- TODO(bernhard): UBERHAX.  ghc patch?
foreign import ccall safe "lookupSymbol"
   c_lookupSymbol :: CString -> IO (Ptr a)

loadNativeFunction :: String -> IO Word32
loadNativeFunction sym = do
  _ <- loadRawObject "ffi/native.o"
  -- TODO(bernhard): WTF
  resolveObjs (return ())
  ptr <- withCString sym c_lookupSymbol
  if ptr == nullPtr
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

addMethodRef :: Word32 -> MethodInfo -> [B.ByteString] -> IO ()
addMethodRef entry (MethodInfo mmname _ msig) clsnames = do
  mmap <- getMethodMap
  let newmap = foldr (\i -> M.insert (MethodInfo mmname i msig) entry) M.empty clsnames
  setMethodMap $ mmap `M.union` newmap


compileBB :: RawMethod -> MethodInfo -> IO Word32
compileBB rawmethod methodinfo = do
  tmap <- getTrapMap

  cls <- getClassFile (methClassName methodinfo)
  let ebb = emitFromBB cls rawmethod
  (_, Right right) <- runCodeGen ebb () ()

  let ((entry, _, _, new_tmap), _) = right
  setTrapMap $ tmap `M.union` new_tmap -- prefers elements in tmap

  printfJit "generated code of \"%s\" from \"%s\":\n" (toString $ methName methodinfo) (toString $ methClassName methodinfo)
  printfJit "\tstacksize: 0x%04x, locals: 0x%04x\n" (rawStackSize rawmethod) (rawLocals rawmethod)
  mapM_ (printfJit "%s\n" . showAtt) (snd right)
  printfJit "\n\n"
  -- UNCOMMENT NEXT LINES FOR GDB FUN
  -- if (toString $ methName methodinfo) == "thejavamethodIwant2debug"
  --   then putStrLn "press CTRL+C now for setting a breakpoint. then `c' and ENTER for continue" >> getLine
  --   else return "foo"
  -- (1) build a debug build (see HACKING) and execute `make tests/Fib.gdb'
  --     for example, where the suffix is important
  -- (2) on getLine, press CTRL+C
  -- (3) `br *0x<addr>'; obtain the address from the disasm above
  -- (4) `cont' and press enter
  return $ fromIntegral $ ptrToIntPtr entry


executeFuncPtr :: Word32 -> IO ()
executeFuncPtr entry =
  code_void ((castPtrToFunPtr $ intPtrToPtr $ fromIntegral entry) :: FunPtr (IO ()))
