{-# LANGUAGE OverloadedStrings #-}
module Mate.X86CodeGen where

import Data.Binary
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B

import Foreign
import Foreign.Ptr
import Foreign.C.Types

import Text.Printf

import qualified JVM.Assembler as J
import JVM.Assembler hiding (Instruction)

import Harpy
import Harpy.X86Disassembler

import Mate.BasicBlocks

test_01 = do
      hmap <- parseMethod "./tests/While.class" "f"
      printMapBB hmap
      case hmap of
        Nothing -> putStrLn "sorry, no code generation"
        Just hmap -> do
              let ebb = emitFromBB hmap
              (_, Right ((entry, bbstarts, jumps), disasm)) <- runCodeGen ebb () ()
              let int_entry = ((fromIntegral $ ptrToIntPtr entry) :: Int)
              -- TODO(bernhard): patch jumps
              printf "disasm:\n"
              mapM_ (putStrLn . showAtt) disasm
              printf "basicblocks addresses:\n"
              let b = map (\(x,y) -> (x,y + int_entry)) $ M.toList bbstarts
              mapM_ (\(x,y) -> printf "\tBasicBlock %2d starts at 0x%08x\n" x y) b
              printf "stuff to patch:\n"
              let patching = [ (int_entry + topatch
                             ,  int_entry + (fromJust $ M.lookup bid bbstarts))
                             | (Jump (bid,topatch)) <- jumps]
              mapM_ (\(x,y) -> printf "\tpatch jmp @ 0x%08x to address 0x%08x\n" x y) patching

type EntryPoint = Ptr Word8
type EntryPointOffset = Int
type PatchInfo = (BlockID, EntryPointOffset)
data Jump = Jump PatchInfo

type BBStarts = M.Map BlockID Int

type CompileInfo = (EntryPoint, BBStarts, [Jump])

emitFromBB :: MapBB -> CodeGen e s (CompileInfo, [Instruction])
emitFromBB hmap =  do
        ep <- getEntryPoint
        push ebp
        mov ebp esp
        (bbstarts, jumps) <- efBB (0,(hmap M.! 0)) M.empty
        mov esp ebp
        pop ebp
        ret
        d <- disassemble
        return ((ep, bbstarts, jumps), d)
  where
  efBB :: (BlockID, BasicBlock) -> BBStarts -> CodeGen e s (BBStarts, [Jump])
  efBB (bid, bb) bbstarts =
        if M.member bid bbstarts then
          return (bbstarts, [])
        else
          do
          bb_offset <- getCodeOffset
          let bbstarts' = M.insert bid bb_offset bbstarts
          mapM emit $ code bb
          jj <- getCodeOffset
          let j = Jump (bid, jj)
          case successor bb of
            Return -> return (bbstarts', [])
            OneTarget t -> do
              (bbstarts'', jumps) <- efBB (t, hmap M.! t) bbstarts'
              return (bbstarts'', j:jumps)
            TwoTarget t1 t2 -> do
              (bbstarts'', jumps) <- efBB (t1, hmap M.! t1) bbstarts'
              (bbstarts''', jumps') <- efBB (t2, hmap M.! t2) bbstarts''
              return (bbstarts''', j:(jumps ++ jumps'))
  -- TODO(bernhard): also use metainformation
  -- TODO(bernhard): implement `emit' as function which accepts a list of
  --                 instructions, so we can use patterns for optimizations
  emit :: J.Instruction -> CodeGen e s ()
  emit (ILOAD_ x) = do
      push (Disp (cArgs_ x), ebp)
  emit (ISTORE_ x) = do
      pop eax
      mov (Disp (cArgs_ x), ebp) eax
  emit IADD = do pop ebx; pop eax; add eax ebx; push eax
  emit (IINC x imm) = do
      add (Disp (cArgs x), ebp) (s8_w32 imm)

  emit (IF cond _) = do
      pop eax
      cmp eax (0 :: Word32)
      -- TODO(bernhard): can we use harpy magic here, in order to avoid patching?
      case cond of
        -- "patch me" after code generation (here we don't know the address yet)
        C_EQ -> error "not implemented yet"
        C_NE -> error "not implemented yet"
        C_LT -> error "not implemented yet"
        C_GE -> error "not implemented yet"
        C_GT -> jg (0xaabbccdd :: Word32)
        C_LE -> error "not implemented yet"

  emit IRETURN = do pop eax
  emit _ = do cmovbe eax eax -- dummy

  cArgs x = (8 + 4 * (fromIntegral x))
  cArgs_ x = (8 + 4 * case x of I0 -> 0; I1 -> 1; I2 -> 2; I3 -> 3)

  -- sign extension from w8 to w32 (over s8)
  --   unfortunately, hs-java is using Word8 everywhere (while
  --   it should be Int8 actually)
  s8_w32 :: Word8 -> Word32
  s8_w32 w8 = fromIntegral s8
    where s8 = (fromIntegral w8) :: Int8
