{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
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

foreign import ccall "dynamic"
   code_int :: FunPtr (CInt -> CInt -> IO CInt) -> (CInt -> CInt -> IO CInt)

foreign import ccall "getaddr"
  getaddr :: CUInt

foreign import ccall "callertrap"
  callertrap :: IO ()

foreign import ccall "register_signal"
  register_signal :: IO ()

test_01, test_02, test_03 :: IO ()
test_01 = do
  register_signal
  (entry, end) <- testCase "./tests/Fib.class" "fib"
  let entryFuncPtr = ((castPtrToFunPtr entry) :: FunPtr (CInt -> CInt -> IO CInt))

  mapM_ (\(x,entryFuncPtr) -> do
    result <- code_int entryFuncPtr (fromIntegral x) (fromIntegral 0)
    let iresult :: Int; iresult = fromIntegral result
    let kk :: String; kk = if iresult == (fib x) then "OK" else "FAIL (" ++ (show (fib x)) ++ ")"
    printf "result of fib(%2d): %3d\t\t%s\n" x iresult kk
    ) $ zip ([0..10] :: [Int]) (repeat entryFuncPtr)
  printf "patched disasm:\n"
  Right newdisasm <- disassembleBlock entry end
  mapM_ (putStrLn . showAtt) newdisasm
  where
    fib n
      | n <= 1 = 1
      | otherwise = (fib (n - 1)) + (fib (n - 2))


test_02 = do
  (entry,_) <- testCase "./tests/While.class" "f"
  let entryFuncPtr = ((castPtrToFunPtr entry) :: FunPtr (CInt -> CInt -> IO CInt))
  result <- code_int entryFuncPtr (fromIntegral 5) (fromIntegral 4)
  let iresult :: Int; iresult = fromIntegral result
  let kk :: String; kk = if iresult == 15 then "OK" else "FAIL"
  printf "result of f(5,4): %3d\t\t%s\n" iresult kk

  result <- code_int entryFuncPtr (fromIntegral 4) (fromIntegral 3)
  let iresult :: Int; iresult = fromIntegral result
  let kk :: String; kk = if iresult == 10 then "OK" else "FAIL"
  printf "result of f(4,3): %3d\t\t%s\n" iresult kk


test_03 = do
  (entry,_) <- testCase "./tests/While.class" "g"
  let entryFuncPtr = ((castPtrToFunPtr entry) :: FunPtr (CInt -> CInt -> IO CInt))
  result <- code_int entryFuncPtr (fromIntegral 5) (fromIntegral 4)
  let iresult :: Int; iresult = fromIntegral result
  let kk :: String; kk = if iresult == 15 then "OK" else "FAIL"
  printf "result of g(5,4): %3d\t\t%s\n" iresult kk

  result <- code_int entryFuncPtr (fromIntegral 4) (fromIntegral 3)
  let iresult :: Int; iresult = fromIntegral result
  let kk :: String; kk = if iresult == 10 then "OK" else "FAIL"
  printf "result of g(4,3): %3d\t\t%s\n" iresult kk


testCase :: String -> B.ByteString -> IO (Ptr Word8, Int)
testCase cf method = do
      hmap <- parseMethod cf method
      printMapBB hmap
      case hmap of
        Nothing -> error "sorry, no code generation"
        Just hmap -> do
              let ebb = emitFromBB hmap
              (_, Right ((entry, bbstarts, end), disasm)) <- runCodeGen ebb () ()
              let int_entry = ((fromIntegral $ ptrToIntPtr entry) :: Int)
              printf "disasm:\n"
              mapM_ (putStrLn . showAtt) disasm
              printf "basicblocks addresses:\n"
              let b = map (\(x,y) -> (x,y + int_entry)) $ M.toList bbstarts
              mapM_ (\(x,y) -> printf "\tBasicBlock %2d starts at 0x%08x\n" x y) b
              return (entry, end)

type EntryPoint = Ptr Word8
type EntryPointOffset = Int
type PatchInfo = (BlockID, EntryPointOffset)

type BBStarts = M.Map BlockID Int

type CompileInfo = (EntryPoint, BBStarts, Int)

emitFromBB :: MapBB -> CodeGen e s (CompileInfo, [Instruction])
emitFromBB hmap =  do
        llmap <- sequence [newNamedLabel ("bb_" ++ show x) | (x,_) <- M.toList hmap]
        let lmap = zip (Prelude.fst $ unzip $ M.toList hmap) llmap
        ep <- getEntryPoint
        push ebp
        mov ebp esp

        -- TODO(bernhard): remove me. just for PoC here
        ep <- getEntryPoint
        let w32_ep = (fromIntegral $ ptrToIntPtr ep) :: Word32
        push w32_ep
        -- '5' is the size of the `call' instruction ( + immediate)
        calladdr <- getCodeOffset
        let w32_calladdr = 5 + w32_ep + (fromIntegral calladdr) :: Word32
        let trapaddr = (fromIntegral getaddr :: Word32)
        call (trapaddr - w32_calladdr)

        bbstarts <- efBB (0,(hmap M.! 0)) M.empty lmap
        d <- disassemble
        end <- getCodeOffset
        return ((ep, bbstarts, end), d)
  where
  getLabel :: BlockID -> [(BlockID, Label)] -> Label
  getLabel _ [] = error "label not found!"
  getLabel i ((x,l):xs) = if i==x then l else getLabel i xs

  efBB :: (BlockID, BasicBlock) -> BBStarts -> [(BlockID, Label)] -> CodeGen e s (BBStarts)
  efBB (bid, bb) bbstarts lmap =
        if M.member bid bbstarts then
          return bbstarts
        else do
          bb_offset <- getCodeOffset
          let bbstarts' = M.insert bid bb_offset bbstarts
          defineLabel $ getLabel bid lmap
          mapM emit $ code bb
          case successor bb of
            Return -> return bbstarts'
            OneTarget t -> do
              efBB (t, hmap M.! t) bbstarts' lmap
            TwoTarget t1 t2 -> do
              bbstarts'' <- efBB (t1, hmap M.! t1) bbstarts' lmap
              efBB (t2, hmap M.! t2) bbstarts'' lmap
    -- TODO(bernhard): also use metainformation
    -- TODO(bernhard): implement `emit' as function which accepts a list of
    --                 instructions, so we can use patterns for optimizations
    where
    emit :: J.Instruction -> CodeGen e s ()
    emit (ICONST_1) = push (1 :: Word32)
    emit (ICONST_2) = push (2 :: Word32)
    emit (ILOAD_ x) = do
        push (Disp (cArgs_ x), ebp)
    emit (ISTORE_ x) = do
        pop eax
        mov (Disp (cArgs_ x), ebp) eax
    emit IADD = do pop ebx; pop eax; add eax ebx; push eax
    emit ISUB = do pop ebx; pop eax; sub eax ebx; push eax
    emit (IINC x imm) = do
        add (Disp (cArgs x), ebp) (s8_w32 imm)

    emit (IF_ICMP cond _) = do
        pop eax -- value2
        pop ebx -- value1
        cmp ebx eax -- intel syntax is swapped (TODO(bernhard): test that plz)
        let sid = case successor bb of TwoTarget _ t -> t
        let l = getLabel sid lmap
        case cond of
          C_EQ -> je  l; C_NE -> jne l
          C_LT -> jl  l; C_GT -> jg  l
          C_GE -> jge l; C_LE -> jle l

    emit (IF cond _) = do
        pop eax -- value1
        cmp eax (0 :: Word32) -- TODO(bernhard): test that plz
        let sid = case successor bb of TwoTarget _ t -> t
        let l = getLabel sid lmap
        case cond of
          C_EQ -> je  l; C_NE -> jne l
          C_LT -> jl  l; C_GT -> jg  l
          C_GE -> jge l; C_LE -> jle l

    emit (GOTO _ ) = do
        let sid = case successor bb of OneTarget t -> t
        jmp $ getLabel sid lmap
    emit (INVOKESTATIC x) = do
        -- TODO(bernhard): get and save information about this call
        -- TODO(bernhard): better try SIGILL instead of SIGSEGV?
        mov (Addr 0) eax
        -- discard arguments (TODO(bernhard): don't hardcode it)
        add esp (4 :: Word32)
        -- push result on stack (TODO(bernhard): if any)
        push eax

    emit IRETURN = do
        pop eax
        mov esp ebp
        pop ebp
        ret
    emit _ = do cmovbe eax eax -- dummy

  cArgs x = (8 + 4 * (fromIntegral x))
  cArgs_ x = (8 + 4 * case x of I0 -> 0; I1 -> 1; I2 -> 2; I3 -> 3)

  -- sign extension from w8 to w32 (over s8)
  --   unfortunately, hs-java is using Word8 everywhere (while
  --   it should be Int8 actually)
  s8_w32 :: Word8 -> Word32
  s8_w32 w8 = fromIntegral s8
    where s8 = (fromIntegral w8) :: Int8