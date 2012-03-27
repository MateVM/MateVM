{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Data.Binary
import Data.String
import System.Environment hiding (getEnv)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B

import Text.Printf

import Control.Monad

import qualified JVM.Assembler as J
import JVM.Assembler hiding (Instruction)
import JVM.Common
import JVM.ClassFile
import JVM.Converter
import JVM.Dump

import Foreign
import Foreign.Ptr
import Foreign.C.Types

import Harpy
import Harpy.X86Disassembler

import Utilities

foreign import ccall "dynamic"
   code_void :: FunPtr (CInt -> IO CInt) -> (CInt -> IO CInt)

foreign import ccall "getaddr"
  getaddr :: CUInt

foreign import ccall "callertrap"
  callertrap :: IO ()


$(callDecl "callAsWord32" [t|Word32|])

main = do
  args <- getArgs
  case args of
    [clspath] -> do
      clsFile <- decodeFile clspath
      let cp = constsPool (clsFile :: Class Pointers)
      putStrLn "==== constpool: ===="
      putStrLn $ showListIx $ M.elems cp
      cf <- parseClassFile clspath
      putStrLn "==== classfile dump: ===="
      dumpClass cf
      putStrLn "==== random stuff: ===="
      let mainmethod = lookupMethod "main" cf -- "main|([Ljava/lang/String;)V" cf
      case mainmethod of
        Nothing -> putStrLn "no main found"
        Just main ->
          case attrByName main "Code" of
            Nothing -> putStrLn "no code attr found"
            Just bytecode -> do
              putStrLn "woot, running now"
              allocaArray 26 (\ p -> mapM_ (\ i -> poke (advancePtr p i) 0) [0..25] >> runstuff p bytecode)
    _ -> error "Synopsis: dump-class File.class"

runstuff :: Ptr Int32 -> B.ByteString -> IO ()
runstuff env bytecode = do
          let emittedcode = (compile (fromIntegral getaddr)) $ codeInstructions $ decodeMethod bytecode
          (_, Right ((entryPtr, endOffset), disasm)) <- runCodeGen emittedcode env ()
          printf "entry point: 0x%08x\n" ((fromIntegral $ ptrToIntPtr entryPtr) :: Int)

          
          let entryFuncPtr = ((castPtrToFunPtr entryPtr) :: FunPtr (CInt -> IO CInt))
          printf "got ptr\n"
          result <- code_void entryFuncPtr (fromIntegral 0x1337)
          printf "called code_void\n"
          let iresult::Int; iresult = fromIntegral result
          printf "result: 0x%08x\n" iresult -- expecting (2 * 0x1337) + 0x42 = 0x26b0

          result2 <- code_void entryFuncPtr (fromIntegral (-0x20))
          let iresult2::Int; iresult2 = fromIntegral result2
          printf "result: 0x%08x\n" iresult2 -- expecting 0x2


          -- s/mov ebx 0x6666/mov eax 0x6666/
          let patchit = plusPtr entryPtr 0xb
          poke patchit (0xb8 :: Word8)

          result3 <- code_void entryFuncPtr (fromIntegral 0)
          let iresult3::Int; iresult3 = fromIntegral result3
          printf "result: 0x%08x\n" iresult3 -- expecting 0x6666

          printf "disasm:\n"
          mapM_ (putStrLn . showAtt) disasm

          printf "patched disasm:\n"
          Right newdisasm <- disassembleBlock entryPtr endOffset
          mapM_ (putStrLn . showAtt) $ newdisasm

          let addr :: Int; addr = (fromIntegral getaddr :: Int)
          printf "getaddr: 0x%08x\n" addr

          return ()


entryCode :: CodeGen e s ()
entryCode = do push ebp
               mov ebp esp

exitCode :: CodeGen e s ()
exitCode = do mov esp ebp
              pop ebp
              ret

compile :: Word32 -> [J.Instruction] -> CodeGen (Ptr Int32) s ((Ptr Word8, Int), [Instruction])
compile trapaddr insn = do
  entryCode
  mapM compile_ins insn
  push eax
  mov ecx (trapaddr :: Word32)
  call ecx
  -- call trapaddr -- Y U DON'T WORK? (ask mr. gdb for help)
  pop eax
  exitCode
  d <- disassemble
  c <- getEntryPoint
  end <- getCodeOffset
  return ((c,end),d)

compile_ins :: J.Instruction -> CodeGen (Ptr Int32) s ()
compile_ins (BIPUSH w8) = do mov eax ((fromIntegral w8) :: Word32)
compile_ins (PUTSTATIC w16) = do add eax (Disp 8, ebp) -- add first argument to %eax
compile_ins (GETSTATIC w16) = do nop
compile_ins ICONST_2 = do mov ebx (0x6666 :: Word32) -- patch me!
compile_ins IMUL = do nop
  -- mov eax (0 :: Word32)
  -- jmp eax
compile_ins RETURN = do nop
compile_ins _ = do nop

