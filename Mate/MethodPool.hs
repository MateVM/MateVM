{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Mate.MethodPool where

import Data.Binary
import Data.String.Utils
import qualified Data.Map as M
import qualified Data.Bimap as BI
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B
import System.Plugins
import Control.Monad

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import JVM.ClassFile

import Harpy
import Harpy.X86Disassembler

import Mate.BasicBlocks
import Mate.Types
import Mate.NativeMachine
import Mate.ClassPool
import Mate.Debug
import Mate.Utilities
import Mate.Rts()

foreign import ccall "dynamic"
   code_void :: FunPtr (IO ()) -> IO ()

foreign import ccall "&printMemoryUsage"
  printMemoryUsageAddr :: FunPtr (IO ())
 
foreign import ccall "&loadLibrary"
  loadLibraryAddr :: FunPtr (IO ())

foreign import ccall "&printGCStats"
  printGCStatsAddr :: FunPtr (IO ())

getMethodEntry :: MethodInfo -> IO (CPtrdiff, JpcNpcMap)
getMethodEntry mi@(MethodInfo method cm sig) = do
  mmap <- getMethodMap

  (entryaddr, jnmap) <- case M.lookup mi mmap of
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
                if scm == "jmate/lang/MateRuntime" then
                  case smethod of
                    "loadLibrary" ->
                       return (funPtrToAddr loadLibraryAddr, BI.empty)
                    "printGCStats" ->
                       return (funPtrToAddr printGCStatsAddr, BI.empty)
                    "printMemoryUsage" ->
                       return (funPtrToAddr printMemoryUsageAddr, BI.empty)
                    _ ->
                       error $ "native-call: " ++ smethod ++ " not found."
                else do
                  -- TODO(bernhard): cleaner please... *do'h*
                  let sym1 = replace "/" "_" scm
                      parenth = replace "(" "_" $ replace ")" "_" $ toString $ encode sig
                      sym2 = replace ";" "_" $ replace "/" "_" parenth
                      symbol = sym1 ++ "__" ++ smethod ++ "__" ++ sym2
                  printfMp $ printf "native-call: symbol: %s\n" symbol
                  nf <- loadNativeFunction symbol
                  let nf' = (nf, BI.empty)
                  setMethodMap $ M.insert mi nf' mmap
                  return nf'
              else do
                rawmethod <- parseMethod cls' method sig
                entry <- compileBB mi rawmethod (MethodInfo method (thisClass cls') sig)
                addMethodRef entry mi clsnames
                return entry
        Nothing -> error $ show method ++ " not found. abort"
    Just w32 -> return w32
  return (fromIntegral entryaddr, jnmap)

funPtrToAddr :: Num b => FunPtr a -> b
funPtrToAddr = fromIntegral . ptrToIntPtr . castFunPtrToPtr

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

addMethodRef :: (NativeWord, JpcNpcMap) -> MethodInfo -> [B.ByteString] -> IO ()
addMethodRef entry (MethodInfo mmname _ msig) clsnames = do
  mmap <- getMethodMap
  let newmap = foldr (\i -> M.insert (MethodInfo mmname i msig) entry) M.empty clsnames
  setMethodMap $ mmap `M.union` newmap


compileBB :: MethodInfo -> RawMethod -> MethodInfo -> IO (NativeWord, JpcNpcMap)
compileBB mi rawmethod methodinfo = do
  tmap <- getTrapMap

  cls <- getClassFile (methClassName methodinfo)
  printfJit $ printf "emit code of \"%s\" from \"%s\":\n" (toString $ methName methodinfo) (toString $ methClassName methodinfo)
  let ebb = emitFromBB cls mi rawmethod
  let cgconfig = defaultCodeGenConfig { codeBufferSize = fromIntegral $ rawCodeLength rawmethod * 32 }
  (jnmap, Right right) <- runCodeGenWithConfig ebb () BI.empty cgconfig

  let ((entry, _, new_tmap), _) = right
  setTrapMap $ tmap `M.union` new_tmap -- prefers elements in tmap

  printfJit $ printf "generated code of \"%s\" from \"%s\":\n" (toString $ methName methodinfo) (toString $ methClassName methodinfo)
  printfJit $ printf "\tstacksize: 0x%04x, locals: 0x%04x\n" (rawStackSize rawmethod) (rawLocals rawmethod)
  when mateDEBUG $ mapM_ (printfJit . printf "%s\n" . showIntel) (snd right)
  printfJit $ printf "\n\n"
  -- UNCOMMENT NEXT LINES FOR GDB FUN
  -- if (toString $ methName methodinfo) == "thejavamethodIwant2debug"
  --   then putStrLn "press CTRL+C now for setting a breakpoint. then `c' and ENTER for continue" >> getLine
  --   else return "foo"
  -- (1) build a debug build (see HACKING) and execute `make tests/Fib.gdb'
  --     for example, where the suffix is important
  -- (2) on getLine, press CTRL+C
  -- (3) `br *0x<addr>'; obtain the address from the disasm above
  -- (4) `cont' and press enter
  return (fromIntegral $ ptrToIntPtr entry, jnmap)


executeFuncPtr :: NativeWord -> IO ()
executeFuncPtr entry =
  code_void ((castPtrToFunPtr $ intPtrToPtr $ fromIntegral entry) :: FunPtr (IO ()))
