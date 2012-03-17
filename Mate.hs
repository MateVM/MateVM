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


foreign import ccall "dynamic"
   code_void :: FunPtr (CInt -> IO CInt) -> (CInt -> IO CInt)


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
          let emittedcode = compile $ codeInstructions $ decodeMethod bytecode
          (_, Right (entryPtr, disasm)) <- runCodeGen emittedcode env ()
          printf "entry point: 0x%08x\n" ((fromIntegral $ ptrToIntPtr entryPtr) :: Int)

          let entryFuncPtr = ((castPtrToFunPtr entryPtr) :: FunPtr (CInt -> IO CInt))
          result <- code_void entryFuncPtr (fromIntegral 0x1337)
          let iresult::Int; iresult = fromIntegral result
          printf "result: 0x%08x\n" iresult

          result2 <- code_void entryFuncPtr (fromIntegral (-0x20))
          let iresult2::Int; iresult2 = fromIntegral result2
          printf "result: 0x%08x\n" iresult2

          printf "disasm:\n"
          mapM_ (putStrLn . showAtt) disasm
          return ()


entryCode :: CodeGen e s ()
entryCode = do push ebp
               mov ebp esp

exitCode :: CodeGen e s ()
exitCode = do mov esp ebp
              pop ebp
              ret

compile :: [J.Instruction] -> CodeGen (Ptr Int32) s (Ptr Word8, [Instruction])
compile insn = do
  entryCode
  mapM compile_ins insn
  exitCode
  d <- disassemble
  c <- getEntryPoint
  return (c,d)

compile_ins :: J.Instruction -> CodeGen (Ptr Int32) s ()
compile_ins (BIPUSH w8) = do mov eax ((fromIntegral w8) :: Word32)
compile_ins (PUTSTATIC w16) = do add eax (Disp 8, ebp) -- add first argument to %eax
compile_ins (GETSTATIC w16) = do nop
compile_ins ICONST_2 = do nop
compile_ins IMUL = do nop
compile_ins RETURN = do nop
compile_ins _ = do nop

-- TODO: actually this function already exists in hs-java-0.3!
lookupMethod :: B.ByteString -> Class Resolved -> Maybe (Method Resolved)
lookupMethod name cls = look (classMethods cls)
  where
    look [] = Nothing
    look (f:fs)
      | methodName f == name = Just f
      | otherwise  = look fs
