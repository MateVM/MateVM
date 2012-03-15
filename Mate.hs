{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
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

import Harpy
import Harpy.X86Disassembler


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
          (_, Right (ret, disasm)) <- runCodeGen (compile $ codeInstructions $ decodeMethod bytecode) env ()
          printf "return value: 0x%08x\n" ret
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

compile :: [J.Instruction] -> CodeGen (Ptr Int32) s (Int32, [Instruction])
compile insn = do
  entryCode
  mapM compile_ins insn
  exitCode
  d <- disassemble
  r <- callAsWord32
  return (fromIntegral r, d)

compile_ins :: J.Instruction -> CodeGen (Ptr Int32) s ()
compile_ins (BIPUSH w8) = do mov eax ((fromIntegral w8) :: Word32)
compile_ins (PUTSTATIC w16) = do nop
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
