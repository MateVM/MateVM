{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#include "debug.h"
module Mate.X86CodeGen where

import Prelude hiding (and, div)
import Data.Binary
import Data.BinaryState
import Data.Int
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B
import Control.Monad

import Foreign hiding (xor)
import Foreign.C.Types

import qualified JVM.Assembler as J
import JVM.Assembler hiding (Instruction)
import JVM.ClassFile

import Harpy
import Harpy.X86Disassembler

import Mate.BasicBlocks
import Mate.Types
import Mate.Utilities
import Mate.ClassPool
import Mate.Strings
#ifdef DEBUG
import Text.Printf
#endif


foreign import ccall "&mallocObject"
  mallocObjectAddr :: FunPtr (Int -> IO CUInt)

type EntryPoint = Ptr Word8
type EntryPointOffset = Int
type PatchInfo = (BlockID, EntryPointOffset)

type BBStarts = M.Map BlockID Int

type CompileInfo = (EntryPoint, BBStarts, Int, TrapMap)


emitFromBB :: B.ByteString -> MethodSignature -> Class Direct -> MapBB -> CodeGen e s (CompileInfo, [Instruction])
emitFromBB method sig cls hmap =  do
        llmap <- sequence [newNamedLabel ("bb_" ++ show x) | (x,_) <- M.toList hmap]
        let lmap = zip (Prelude.fst $ unzip $ M.toList hmap) llmap
        ep <- getEntryPoint
        push ebp
        mov ebp esp
        -- TODO(bernhard): determine a reasonable value.
        --                 e.g. (locals used) * 4
        sub esp (0x60 :: Word32)

        (calls, bbstarts) <- efBB (0, hmap M.! 0) M.empty M.empty lmap
        d <- disassemble
        end <- getCodeOffset
        return ((ep, bbstarts, end, calls), d)
  where
  getLabel :: BlockID -> [(BlockID, Label)] -> Label
  getLabel _ [] = error "label not found!"
  getLabel i ((x,l):xs) = if i==x then l else getLabel i xs

  efBB :: (BlockID, BasicBlock) -> TrapMap -> BBStarts -> [(BlockID, Label)] -> CodeGen e s (TrapMap, BBStarts)
  efBB (bid, bb) calls bbstarts lmap =
        if M.member bid bbstarts then
          return (calls, bbstarts)
        else do
          bb_offset <- getCodeOffset
          let bbstarts' = M.insert bid bb_offset bbstarts
          defineLabel $ getLabel bid lmap
          cs <- mapM emit'' $ code bb
          let calls' = calls `M.union` M.fromList (catMaybes cs)
          case successor bb of
            Return -> return (calls', bbstarts')
            FallThrough t -> efBB (t, hmap M.! t) calls' bbstarts' lmap
            OneTarget t -> efBB (t, hmap M.! t) calls' bbstarts' lmap
            TwoTarget t1 t2 -> do
              (calls'', bbstarts'') <- efBB (t1, hmap M.! t1) calls' bbstarts' lmap
              efBB (t2, hmap M.! t2) calls'' bbstarts'' lmap
    -- TODO(bernhard): also use metainformation
    -- TODO(bernhard): implement `emit' as function which accepts a list of
    --                 instructions, so we can use patterns for optimizations
    where
    getCurrentOffset :: CodeGen e s Word32
    getCurrentOffset = do
      ep <- getEntryPoint
      let w32_ep = (fromIntegral $ ptrToIntPtr ep) :: Word32
      offset <- getCodeOffset
      return $ w32_ep + fromIntegral offset

    emitInvoke :: Word16 -> Bool -> CodeGen e s (Maybe (Word32, TrapCause))
    emitInvoke cpidx hasThis = do
        let l = buildMethodID cls cpidx
        calladdr <- getCurrentOffset
        newNamedLabel (show l) >>= defineLabel
        -- causes SIGILL. in the signal handler we patch it to the acutal call.
        -- place a nop at the end, therefore the disasm doesn't screw up
        emit32 (0xffff9090 :: Word32) >> emit8 (0x90 :: Word8)
        -- discard arguments on stack
        let argcnt = ((if hasThis then 1 else 0) + methodGetArgsCount cls cpidx) * 4
        when (argcnt > 0) (add esp argcnt)
        -- push result on stack if method has a return value
        when (methodHaveReturnValue cls cpidx) (push eax)
        -- +2 is for correcting eip in trap context
        return $ Just (calladdr + 2, StaticMethod l)

    invokeEpilog :: Word16 -> Word32 -> (Bool -> TrapCause) -> CodeGen e s (Maybe (Word32, TrapCause))
    invokeEpilog cpidx offset trapcause = do
        -- make actual (indirect) call
        calladdr <- getCurrentOffset
        call (Disp offset, eax)
        -- discard arguments on stack (+4 for "this")
        let argcnt = 4 + 4 * methodGetArgsCount cls cpidx
        when (argcnt > 0) (add esp argcnt)
        -- push result on stack if method has a return value
        when (methodHaveReturnValue cls cpidx) (push eax)
        let imm8 = is8BitOffset offset
        return $ Just (calladdr + (if imm8 then 3 else 6), trapcause imm8)

    emit'' :: J.Instruction -> CodeGen e s (Maybe (Word32, TrapCause))
    emit'' insn = newNamedLabel ("jvm_insn: " ++ show insn) >>= defineLabel >> emit' insn

    emit' :: J.Instruction -> CodeGen e s (Maybe (Word32, TrapCause))
    emit' (INVOKESPECIAL cpidx) = emitInvoke cpidx True
    emit' (INVOKESTATIC cpidx) = emitInvoke cpidx False
    emit' (INVOKEINTERFACE cpidx _) = do
        -- get methodInfo entry
        let mi@(MethodInfo methodname ifacename msig@(MethodSignature args _)) = buildMethodID cls cpidx
        newNamedLabel (show mi) >>= defineLabel
        -- objref lives somewhere on the argument stack
        mov eax (Disp ((*4) $ fromIntegral $ length args), esp)
        -- get method-table-ptr, keep it in eax (for trap handling)
        mov eax (Disp 0, eax)
        -- get interface-table-ptr
        mov ebx (Disp 0, eax)
        -- get method offset
        offset <- liftIO $ getInterfaceMethodOffset ifacename methodname (encode msig)
        -- note, that "mi" has the wrong class reference here.
        -- we figure that out at run-time, in the methodpool,
        -- depending on the method-table-ptr
        invokeEpilog cpidx offset (\x -> InterfaceMethod x mi)
    emit' (INVOKEVIRTUAL cpidx) = do
        -- get methodInfo entry
        let mi@(MethodInfo methodname objname msig@(MethodSignature args _))  = buildMethodID cls cpidx
        newNamedLabel (show mi) >>= defineLabel
        -- objref lives somewhere on the argument stack
        mov eax (Disp ((*4) $ fromIntegral $ length args), esp)
        -- get method-table-ptr
        mov eax (Disp 0, eax)
        -- get method offset
        let nameAndSig = methodname `B.append` encode msig
        offset <- liftIO $ getMethodOffset objname nameAndSig
        -- note, that "mi" has the wrong class reference here.
        -- we figure that out at run-time, in the methodpool,
        -- depending on the method-table-ptr
        invokeEpilog cpidx offset (\x -> VirtualMethod x mi)
    emit' (PUTSTATIC cpidx) = do
        pop eax
        trapaddr <- getCurrentOffset
        mov (Addr 0x00000000) eax -- it's a trap
        return $ Just (trapaddr, StaticField $ buildStaticFieldID cls cpidx)
    emit' (GETSTATIC cpidx) = do
        trapaddr <- getCurrentOffset
        mov eax (Addr 0x00000000) -- it's a trap
        push eax
        return $ Just (trapaddr, StaticField $ buildStaticFieldID cls cpidx)
    emit' insn = emit insn >> return Nothing

    emit :: J.Instruction -> CodeGen e s ()
    emit POP = add esp (4 :: Word32) -- drop value
    emit DUP = push (Disp 0, esp)
    emit DUP_X1 = do pop eax; pop ebx; push eax; push ebx; push eax
    emit DUP_X2 = do pop eax; pop ebx; pop ecx; push eax; push ecx; push ebx; push eax
    emit AASTORE = emit IASTORE
    emit IASTORE = do
        pop eax -- value
        pop ebx -- offset
        add ebx (1 :: Word32)
        pop ecx -- aref
        mov (ecx, ebx, S4) eax
    emit CASTORE = do
        pop eax -- value
        pop ebx -- offset
        add ebx (1 :: Word32)
        pop ecx -- aref
        mov (ecx, ebx, S1) eax -- TODO(bernhard): char is two byte
    emit AALOAD = emit IALOAD
    emit IALOAD = do
        pop ebx -- offset
        add ebx (1 :: Word32)
        pop ecx -- aref
        push (ecx, ebx, S4)
    emit CALOAD = do
        pop ebx -- offset
        add ebx (1 :: Word32)
        pop ecx -- aref
        push (ecx, ebx, S1) -- TODO(bernhard): char is two byte
    emit ARRAYLENGTH = do
        pop eax
        push (Disp 0, eax)
    emit (ANEWARRAY _) = emit (NEWARRAY 10) -- 10 == T_INT
    emit (NEWARRAY typ) = do
        let tsize = case decodeS (0 :: Integer) (B.pack [typ]) of
                    T_INT -> 4
                    T_CHAR -> 2
                    _ -> error "newarray: type not implemented yet"
        -- get length from stack, but leave it there
        mov eax (Disp 0, esp)
        mov ebx (tsize :: Word32)
        -- multiple amount with native size of one element
        mul ebx -- result is in eax
        add eax (4 :: Word32) -- for "length" entry
        -- push amount of bytes to allocate
        push eax
        callMalloc
        pop eax -- ref to arraymemory
        pop ebx -- length
        mov (Disp 0, eax) ebx -- store length at offset 0
        push eax -- push ref again
    emit (NEW objidx) = do
        let objname = buildClassID cls objidx
        amount <- liftIO $ getObjectSize objname
        push (amount :: Word32)
        callMalloc
        -- TODO(bernhard): save reference somewhere for GC
        -- set method table pointer
        mtable <- liftIO $ getMethodTable objname
        mov (Disp 0, eax) mtable
    emit (CHECKCAST _) = nop -- TODO(bernhard): ...
    -- TODO(bernhard): ...
    emit (INSTANCEOF _) = do
      pop eax
      push (1 :: Word32)
    emit ATHROW = nop -- TODO(bernhard): ...
    emit I2C = do
      pop eax
      and eax (0x000000ff :: Word32)
      push eax
    emit (BIPUSH val) = push (fromIntegral val :: Word32)
    emit (SIPUSH val) = push (fromIntegral (fromIntegral val :: Int16) :: Word32)
    emit ACONST_NULL = push (0 :: Word32)
    emit (ICONST_M1) = push ((-1) :: Word32)
    emit (ICONST_0) = push (0 :: Word32)
    emit (ICONST_1) = push (1 :: Word32)
    emit (ICONST_2) = push (2 :: Word32)
    emit (ICONST_3) = push (3 :: Word32)
    emit (ICONST_4) = push (4 :: Word32)
    emit (ICONST_5) = push (5 :: Word32)
    emit (ALOAD_ x) = emit (ILOAD_ x)
    emit (ILOAD_ x) = push (Disp (cArgs_ x), ebp)
    emit (ALOAD x) = emit (ILOAD x)
    emit (ILOAD x) = push (Disp (cArgs x), ebp)
    emit (ASTORE_ x) = emit (ISTORE_ x)
    emit (ISTORE_ x) = do
        pop eax
        mov (Disp (cArgs_ x), ebp) eax
    emit (ASTORE x) = emit (ISTORE x)
    emit (ISTORE x) = do
        pop eax
        mov (Disp (cArgs x), ebp) eax

    emit (LDC1 x) = emit (LDC2 $ fromIntegral x)
    emit (LDC2 x) = do
        value <- case constsPool cls M.! x of
                      (CString s) -> liftIO $ getUniqueStringAddr s
                      e -> error $ "LDCI... missing impl.: " ++ show e
        push value
    emit (GETFIELD x) = do
        offset <- emitFieldOffset x
        push (Disp (fromIntegral offset), eax) -- get field
    emit (PUTFIELD x) = do
        pop ebx -- value to write
        offset <- emitFieldOffset x
        mov (Disp (fromIntegral offset), eax) ebx -- set field

    emit IADD = do pop ebx; pop eax; add eax ebx; push eax
    emit ISUB = do pop ebx; pop eax; sub eax ebx; push eax
    emit IMUL = do pop ebx; pop eax; mul ebx; push eax
    emit IDIV = do pop ebx; pop eax; xor edx edx; div ebx; push eax
    emit IREM = do pop ebx; pop eax; xor edx edx; div ebx; push edx
    emit IXOR = do pop ebx; pop eax; xor eax ebx; push eax
    emit INEG = do pop eax; neg eax; push eax
    emit (IINC x imm) =
        add (Disp (cArgs x), ebp) (s8_w32 imm)

    emit (IFNONNULL x) = emit (IF C_NE x)
    emit (IFNULL x) = emit (IF C_EQ x)
    emit (IF_ACMP cond x) = emit (IF_ICMP cond x)
    emit (IF_ICMP cond _) = do
        pop eax -- value2
        pop ebx -- value1
        cmp ebx eax -- intel syntax is swapped (TODO(bernhard): test that plz)
        emitIF cond

    emit (IF cond _) = do
        pop eax -- value1
        cmp eax (0 :: Word32) -- TODO(bernhard): test that plz
        emitIF cond

    emit (GOTO _ ) = do
        let sid = case successor bb of OneTarget t -> t; _ -> error "bad"
        jmp $ getLabel sid lmap

    emit RETURN = do mov esp ebp; pop ebp; ret
    emit ARETURN = emit IRETURN
    emit IRETURN = do pop eax; emit RETURN
    emit invalid = error $ "insn not implemented yet: " ++ show invalid

    emitFieldOffset :: Word16 -> CodeGen e s Int32
    emitFieldOffset x = do
        pop eax -- this pointer
        let (cname, fname) = buildFieldOffset cls x
        liftIO $ getFieldOffset cname fname

    emitIF :: CMP -> CodeGen e s ()
    emitIF cond = let
      sid = case successor bb of TwoTarget _ t -> t; _ -> error "bad"
      l = getLabel sid lmap
      in case cond of
        C_EQ -> je  l; C_NE -> jne l
        C_LT -> jl  l; C_GT -> jg  l
        C_GE -> jge l; C_LE -> jle l

    callMalloc :: CodeGen e s ()
    callMalloc = do
        call mallocObjectAddr
        add esp (4 :: Word32)
        push eax

  -- for locals we use a different storage
  cArgs :: Word8 -> Word32
  cArgs x = if x' >= thisMethodArgCnt
      -- TODO(bernhard): maybe s/(-4)/(-8)/
      then fromIntegral $ (-4) * (x' - thisMethodArgCnt + 1)
      else 4 + (thisMethodArgCnt * 4) - (4 * x')
    where x' = fromIntegral x

  cArgs_ :: IMM -> Word32
  cArgs_ x = cArgs $ case x of I0 -> 0; I1 -> 1; I2 -> 2; I3 -> 3

  thisMethodArgCnt :: Word32
  thisMethodArgCnt = isNonStatic + fromIntegral (length args)
    where
    (Just m) = lookupMethodSig method sig cls
    (MethodSignature args _) = sig
    isNonStatic = if S.member ACC_STATIC (methodAccessFlags m)
        then 0 else 1 -- one argument for the this pointer


  -- sign extension from w8 to w32 (over s8)
  --   unfortunately, hs-java is using Word8 everywhere (while
  --   it should be Int8 actually)
  s8_w32 :: Word8 -> Word32
  s8_w32 w8 = fromIntegral s8
    where s8 = fromIntegral w8 :: Int8

  is8BitOffset :: Word32 -> Bool
  is8BitOffset w32 = s32 < 128 && s32 > (-127)
    where s32 = fromIntegral w32 :: Int32
