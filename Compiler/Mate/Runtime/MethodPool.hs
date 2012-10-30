{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Compiler.Mate.Runtime.MethodPool where

import Data.Binary
import Data.String.Utils
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B
import System.Plugins

import Foreign
import Foreign.C.Types
import Foreign.C.String

import JVM.ClassFile

import Compiler.Mate.Debug
import Compiler.Mate.Types

import {-# SOURCE #-} Compiler.Mate.Pipeline
import Compiler.Mate.Backend.NativeSizes
import Compiler.Mate.Utilities
import Compiler.Mate.Runtime.ClassPool
import Compiler.Mate.Runtime.Rts()
import Compiler.Mate.Runtime.JavaObjects()

foreign import ccall "dynamic"
   code_void :: FunPtr (IO ()) -> IO ()

foreign import ccall "&printMemoryUsage"
  printMemoryUsageAddr :: FunPtr (IO ())
 
foreign import ccall "&loadLibrary"
  loadLibraryAddr :: FunPtr (IO ())

foreign import ccall "&printGCStats"
  printGCStatsAddr :: FunPtr (IO ())

foreign import ccall "&cloneObject"
  cloneObjectAddr :: FunPtr (CPtrdiff -> IO CPtrdiff)

getMethodEntry :: MethodInfo -> IO (CPtrdiff, ExceptionMap NativeWord)
getMethodEntry mi@(MethodInfo method cm sig) = do
  mmap <- getMethodMap

  (CompiledMethod entrypoint exmap) <- case M.lookup mi mmap of
    Nothing -> do
      cls <- getClassFile cm
      printfMp $ printf "getMethodEntry: no method \"%s\" found. compile it\n" (show mi)
      mm <- lookupMethodRecursive method sig [] cls
      case mm of
        Just (mm', clsnames, cls') -> do
            let flags = methodAccessFlags mm'
            if S.member ACC_NATIVE flags
              then do
                let scm = toString cm; smethod = toString method
                    ret fp = return $ CompiledMethod (funPtrToAddr fp) M.empty
                case scm of
                  "jmate/lang/MateRuntime" ->
                    case smethod of
                      "loadLibrary" -> ret loadLibraryAddr
                      "printGCStats" -> ret printGCStatsAddr
                      "printMemoryUsage" -> ret printMemoryUsageAddr
                      _ -> error $ "native-call: " ++ smethod ++ " @ " ++ scm ++ " not found."
                  "java/lang/VMObject" ->
                    case smethod of
                      "clone" -> ret cloneObjectAddr
                      _ -> error $ "native-call: " ++ smethod ++ " @ " ++ scm ++ " not found."
                  _ -> do
                    -- TODO(bernhard): cleaner please... *do'h*
                    let sym1 = replace "/" "_" scm
                        parenth = replace "(" "_" $ replace ")" "_" $ toString $ encode sig
                        sym2 = replace ";" "_" $ replace "/" "_" parenth
                        symbol = sym1 ++ "__" ++ smethod ++ "__" ++ sym2
                    printfMp $ printf "native-call: symbol: %s\n" symbol
                    nf <- loadNativeFunction symbol
                    let nf' = CompiledMethod nf M.empty
                    setMethodMap $ M.insert mi nf' mmap
                    return nf'
              else do
                entry <- compile $ MethodInfo method (thisClass cls') sig
                addMethodRef entry mi clsnames
                return entry
        Nothing -> error $ printf "\"%s\" not found. abort" (toString method)
    Just w32 -> return w32
  return (fromIntegral entrypoint, exmap)

funPtrToAddr :: Num b => FunPtr a -> b
funPtrToAddr = fromIntegral . ptrToIntPtr . castFunPtrToPtr

lookupMethodRecursive :: B.ByteString -> MethodSignature -> [B.ByteString] -> Class Direct
                         -> IO (Maybe (Method Direct, [B.ByteString], Class Direct))
lookupMethodRecursive name sig clsnames cls = do
  printfCp $ printf "looking @ %s\n" (toString thisname)
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

loadNativeFunction :: String -> IO NativeWord
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
--   let int_entry = ((fromIntegral $ ptrToIntPtr entry) :: NativeWord)
--   let mmap = M.insert ("fib" :: String) int_entry M.empty
--   mapM_ (\(x,y) -> printf "%s at 0x%08x\n" x y) $ M.toList mmap
--   mmap2ptr mmap >>= set_mmap
--   demo_mmap -- access Data.Map from C

addMethodRef :: CompiledMethod -> MethodInfo -> [B.ByteString] -> IO ()
addMethodRef entry (MethodInfo mmname _ msig) clsnames = do
  mmap <- getMethodMap
  let newmap = foldr (\i -> M.insert (MethodInfo mmname i msig) entry) M.empty clsnames
  setMethodMap $ mmap `M.union` newmap


compile :: MethodInfo -> IO CompiledMethod
compile methodinfo = do
  tmap <- getTrapMap

  cls <- getClassFile (methClassName methodinfo)
  printfJit $ printf "emit code of \"%s\" from \"%s\":\n"
               (toString $ methName methodinfo)
               (toString $ methClassName methodinfo)
  (entry, new_tmap) <- compileMethod (methName methodinfo) cls
  setTrapMap $ tmap `M.union` new_tmap -- prefers elements in tmap
  printfJit $ printf "generated code of \"%s\" @ \"%s\" from \"%s\". DONE\n"
               (toString $ methName methodinfo)
               (show $ methSignature methodinfo)
               (toString $ methClassName methodinfo)

  -- UNCOMMENT NEXT LINES FOR GDB FUN
  -- if (toString $ methName methodinfo) == "thejavamethodIwant2debug"
  --   then putStrLn "press CTRL+C now for setting a breakpoint. then `c' and ENTER for continue" >> getLine
  --   else return "foo"
  -- (1) build a debug build (see HACKING) and execute `make tests/Fib.gdb'
  --     for example, where the suffix is important
  -- (2) on getLine, press CTRL+C
  -- (3) `br *0x<addr>'; obtain the address from the disasm above
  -- (4) `cont' and press enter
  let exmap = undefined
  return $ CompiledMethod entry exmap

executeFuncPtr :: NativeWord -> IO ()
executeFuncPtr entry =
  code_void ((castPtrToFunPtr $ intPtrToPtr $ fromIntegral entry) :: FunPtr (IO ()))
