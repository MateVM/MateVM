{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Compiler.Mate.Runtime.MethodPool
  ( lookupMethodEntry
  , executeFuncPtr
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B
import Data.Binary

import Foreign
import Foreign.C.Types

import JVM.ClassFile

import Compiler.Mate.Debug
import Compiler.Mate.Types

import {-# SOURCE #-} Compiler.Mate.Pipeline
import Compiler.Mate.Backend.NativeSizes
import Compiler.Mate.Utilities
import Compiler.Mate.Runtime.ClassPool
import Compiler.Mate.Runtime.NativeMethods

foreign import ccall "dynamic"
   code_void :: FunPtr (IO ()) -> IO ()


lookupMethodEntry:: MethodInfo -> IO CPtrdiff
lookupMethodEntry mi@(MethodInfo method cm sig) = do
  mmap <- getMethodMap
  case M.lookup mi mmap of
    Nothing -> do
      cls <- getClassFile cm
      printfMp $ printf "getMethodEntry: no method \"%s\" found. compile it\n" (show mi)
      mm <- lookupMethodRecursive method sig [] cls
      case mm of
        Just (mm', clsnames, cls') -> do
            let flags = methodAccessFlags mm'
            nf <- if S.member ACC_NATIVE flags
              then nativeAddr (toString cm) (toString method) (toString $ encode sig)
              else do -- plain java method
                entry <- compile $ MethodInfo method (thisClass cls') sig
                insertCompiledMethod entry mi clsnames
                return $ fromIntegral entry
            setMethodMap $ M.insert mi nf mmap
            return $ fromIntegral nf
        Nothing -> error $ printf "\"%s\" not found. abort" (toString method)
    Just w32 -> return $ fromIntegral w32

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
    res = lookupMethodWithSig name sig cls
    thisname = thisClass cls
    nextclsn :: [B.ByteString]
    nextclsn = thisname:clsnames

insertCompiledMethod :: NativeWord -> MethodInfo -> [B.ByteString] -> IO ()
insertCompiledMethod entry (MethodInfo mmname _ msig) clsnames = do
  mmap <- getMethodMap
  let newmap = foldr
                  (\i -> M.insert (MethodInfo mmname i msig) entry)
                  M.empty
                  clsnames
  setMethodMap $ mmap `M.union` newmap


compile :: MethodInfo -> IO NativeWord
compile methodinfo = do
  tmap <- getTrapMap

  cls <- getClassFile (methClassName methodinfo)
  printfJit $ printf "emit code of \"%s\" from \"%s\":\n"
               (toString $ methName methodinfo)
               (toString $ methClassName methodinfo)
  (entry, new_trapmap) <- compileMethod (methName methodinfo)
                                     (methSignature methodinfo)
                                     cls
  setTrapMap $ tmap `M.union` new_trapmap -- prefers elements in tmap
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
  return entry

executeFuncPtr :: NativeWord -> IO ()
executeFuncPtr entry =
  code_void ((castPtrToFunPtr $ intPtrToPtr $ fromIntegral entry) :: FunPtr (IO ()))
