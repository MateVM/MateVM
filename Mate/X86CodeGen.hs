{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Mate.X86CodeGen where

import Prelude hiding (and, div)
import Data.Binary
import Data.BinaryState
import Data.Int
import Data.Maybe
import Data.List (genericLength, find)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Control.Applicative
import Control.Monad
import Control.Arrow

import Foreign hiding (xor)
import Foreign.C.Types

import qualified JVM.Assembler as J
import JVM.Assembler hiding (Instruction)
import JVM.ClassFile

import Harpy hiding (fst)
import Harpy.X86Disassembler

import Mate.BasicBlocks
import Mate.NativeSizes
import Mate.Types
import Mate.Utilities
import Mate.ClassPool
import Mate.ClassHierarchy
import {-# SOURCE #-} Mate.MethodPool
import Mate.Strings
import Mate.Debug


foreign import ccall "&mallocObjectGC"
  mallocObjectAddr :: FunPtr (Int -> IO CPtrdiff)

type EntryPoint = Ptr Word8
type EntryPointOffset = Int
type PatchInfo = (BlockID, EntryPointOffset)

type BBStarts = M.Map BlockID Int

type CompileInfo = (EntryPoint, Int, TrapMap, ExceptionMap Word32)


emitFromBB :: Class Direct -> MethodInfo -> RawMethod -> CodeGen e JpcNpcMap (CompileInfo, [Instruction])
emitFromBB cls miThis method = do
    let keys = M.keys hmap
    llmap <- mapM (newNamedLabel . (++) "bb_" . show) keys
    let lmap = zip keys llmap
    -- TODO(bernhard): don't jump around in the code... wtf dude!
    pushExceptionMap <- newNamedLabel "pushExceptionMap"
    stacksetup <- newNamedLabel "stacksetup"
    ep <- getEntryPoint
    push ebp
    jmp pushExceptionMap
    defineLabel stacksetup
    mov ebp esp
    sub esp stackalloc
    calls <- M.fromList . catMaybes . concat <$> mapM (efBB lmap) keys
    jpcnpcmap <- getState
    -- replace java program counter with native maschine program counter
    let exmap = M.map (map h) $ M.mapKeys f $ rawExcpMap method
          where
            f (key1, key2) = (&&&) (M.! key1') (M.! key2') jpcnpcmap
              where
                key1' = fromIntegral key1
                key2' = fromIntegral key2
            h = second ((jpcnpcmap M.!) . fromIntegral)
    sptr_exmap <- liftIO $ do
      (fromIntegral . ptrToIntPtr . castStablePtrToPtr) <$> newStablePtr exmap
    pushExceptionMap @@ push (sptr_exmap :: Word32)
    jmp stacksetup
    d <- disassemble
    end <- getCodeOffset
    return ((ep, end, calls, exmap), d)
  where
  hmap = rawMapBB method
  stackalloc = fromIntegral (rawLocals method) * ptrSize :: Word32

  getLabel :: BlockID -> [(BlockID, Label)] -> Label
  getLabel bid [] = error $ "label " ++ show bid ++ " not found"
  getLabel i ((x,l):xs) = if i==x then l else getLabel i xs

  efBB :: [(BlockID, Label)] -> BlockID -> CodeGen e JpcNpcMap [(Maybe (Word32, TrapCause))]
  efBB lmap bid = do
    defineLabel $ getLabel bid lmap
    retval <- mapM emit'' $ code bb
    case successor bb of
        FallThrough t -> do
          -- TODO(bernhard): le dirty hax. see java/lang/Integer.toString(int, int)
          jmp (getLabel t lmap)
        _ -> return ()
    return retval
    where
    bb = hmap M.! bid

    forceRegDump :: CodeGen e s ()
    forceRegDump = do
      push esi
      mov esi (0x13371234 :: Word32)
      mov esi (Addr 0)
      pop esi

    getCurrentOffset :: CodeGen e s Word32
    getCurrentOffset = do
      ep <- (fromIntegral . ptrToIntPtr) <$> getEntryPoint
      offset <- fromIntegral <$> getCodeOffset
      return $ ep + offset

    emitInvoke :: Word16 -> Bool -> CodeGen e s (Maybe (Word32, TrapCause))
    emitInvoke cpidx hasThis = do
      let l = buildMethodID cls cpidx
      newNamedLabel (show l) >>= defineLabel
      -- like: call $0x01234567
      calladdr <- emitSigIllTrap 5
      let patcher reip = do
            (entryAddr, _) <- liftIO $ getMethodEntry l
            call (fromIntegral (entryAddr - (reip + 5)) :: NativeWord)
            return reip
      -- discard arguments on stack
      let argcnt = ((if hasThis then 1 else 0) + methodGetArgsCount (methodNameTypeByIdx cls cpidx)) * ptrSize
      when (argcnt > 0) (add esp argcnt)
      -- push result on stack if method has a return value
      when (methodHaveReturnValue cls cpidx) (push eax)
      return $ Just (calladdr, StaticMethod patcher)

    virtualCall :: Word16 -> Bool -> CodeGen e s (Maybe (Word32, TrapCause))
    virtualCall cpidx isInterface = do
      let mi@(MethodInfo methodname objname msig@(MethodSignature args _))  = buildMethodID cls cpidx
      newNamedLabel (show mi) >>= defineLabel
      -- get method offset for call @ runtime
      let offset = if isInterface
          then getInterfaceMethodOffset objname methodname (encode msig)
          else getMethodOffset objname (methodname `B.append` encode msig)
      let argsLen = genericLength args
      -- objref lives somewhere on the argument stack
      mov ebx (Disp (argsLen * ptrSize), esp)
      when isInterface $
        mov ebx (Disp 0, ebx) -- get method-table-ptr, keep it in ebx
      -- get method-table-ptr (or interface-table-ptr)
      mov eax (Disp 0, ebx)
      -- make actual (indirect) call
      calladdr <- getCurrentOffset
      -- will be patched to this: call (Disp 0xXXXXXXXX, eax)
      emitSigIllTrap 6
      -- discard arguments on stack (`+1' for "this")
      let argcnt = ptrSize * (1 + methodGetArgsCount (methodNameTypeByIdx cls cpidx))
      when (argcnt > 0) (add esp argcnt)
      -- push result on stack if method has a return value
      when (methodHaveReturnValue cls cpidx) (push eax)
      -- note, that "mi" has the wrong class reference here.
      -- we figure that out at run-time, in the methodpool,
      -- depending on the method-table-ptr
      return $ Just (calladdr, VirtualCall isInterface mi offset)

    emit'' :: (Int, J.Instruction) -> CodeGen e JpcNpcMap (Maybe (Word32, TrapCause))
    emit'' (jpc, insn) = do
      npc <- getCurrentOffset
      jpcnpc <- getState
      newNamedLabel ("jvm_insn: " ++ show insn) >>= defineLabel
      res <- emit' insn
      setState (M.insert jpc npc jpcnpc)
      return res

    emit' :: J.Instruction -> CodeGen e s (Maybe (Word32, TrapCause))
    emit' (INVOKESPECIAL cpidx) = emitInvoke cpidx True
    emit' (INVOKESTATIC cpidx) = emitInvoke cpidx False
    emit' (INVOKEINTERFACE cpidx _) = virtualCall cpidx True
    emit' (INVOKEVIRTUAL cpidx) = virtualCall cpidx False

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

    emit' (GETFIELD x) = do
      pop eax -- this pointer
      -- like: 099db064  ff b0 e4 14 00 00 pushl  5348(%eax)
      trapaddr <- emitSigIllTrap 6
      let patcher reip = do
            let (cname, fname) = buildFieldOffset cls x
            offset <- liftIO $ fromIntegral <$> getFieldOffset cname fname
            push32RelEax (Disp offset) -- get field
            return reip
      return $ Just (trapaddr, ObjectField patcher)
    emit' (PUTFIELD x) = do
      pop ebx -- value to write
      pop eax -- this pointer
      -- like: 4581fc6b  89 98 30 7b 00 00 movl   %ebx,31536(%eax)
      trapaddr <- emitSigIllTrap 6
      let patcher reip = do
            let (cname, fname) = buildFieldOffset cls x
            offset <- liftIO $ fromIntegral <$> getFieldOffset cname fname
            mov32RelEbxEax (Disp offset) -- set field
            return reip
      return $ Just (trapaddr, ObjectField patcher)

    emit' (INSTANCEOF cpidx) = do
      pop eax
      -- place something like `mov edx $mtable_of_objref' instead
      trapaddr <- emitSigIllTrap 4
      push (0 :: Word32)
      let patcher reax reip = do
            emitSigIllTrap 4
            let classname = buildClassID cls cpidx
            check <- liftIO $ isInstanceOf (fromIntegral reax) classname
            if check
              then push (1 :: Word32)
              else push (0 :: Word32)
            return (reip + 4)
      return $ Just (trapaddr, InstanceOf patcher)
    emit' (NEW objidx) = do
      let objname = buildClassID cls objidx
      -- place something like `push $objsize' instead
      trapaddr <- emitSigIllTrap 5
      callMalloc
      -- 0x13371337 is just a placeholder; will be replaced with mtable ptr
      mov (Disp 0, eax) (0x13371337 :: Word32)
      mov (Disp 4, eax) (0x1337babe :: Word32)
      let patcher reip = do
            objsize <- liftIO $ getObjectSize objname
            push32 objsize
            callMalloc
            mtable <- liftIO $ getMethodTable objname
            mov (Disp 0, eax) mtable
            mov (Disp 4, eax) (0x1337babe :: Word32)
            return reip
      return $ Just (trapaddr, NewObject patcher)

    emit' ATHROW = do
      mov eax (Disp 0, esp) -- peek value from stack
      trapaddr <- emitSigIllTrap 2
      let patcher :: TrapPatcherEaxEbp
          patcher reax rebp reip = do
            let weax = fromIntegral reax :: Word32
            let weip = fromIntegral reip :: Word32
            ebpstuff <- liftIO $ do
              peek (intPtrToPtr . fromIntegral $ rebp) :: IO Word32
            liftIO $ printfEx $ printf "read ebp stuff: 0x%08x\n" ebpstuff
            liftIO $ printfEx $ printf "reip: %d\n" weip
            liftIO $ printfEx $ printf "reax: %d\n" weax
            -- get full exception map
            -- (_, exmap) <- liftIO $ getMethodEntry miThis
            let sptr = castPtrToStablePtr $ intPtrToPtr $ fromIntegral ebpstuff
            exmap <- liftIO $ (deRefStablePtr sptr :: IO (ExceptionMap Word32))
            liftIO $ printfEx $ printf "size: %d\n" (M.size exmap)
            liftIO $ printfEx $ printf "exmap: %s\n" (show $ M.toList exmap)
            let key =
                  case find f $ M.keys exmap of
                    Just x -> x
                    Nothing -> error "exception: no handler found. (TODO1)"
                  where
                    -- is the EIP somewhere in the range?
                    f (x, y) = weip >= x && weip <= y
            liftIO $ printfEx $ printf "key is: %s\n" (show key)
            let handlerObjs = exmap M.! key
            liftIO $ printfEx $ printf "handlerObjs: %s\n" (show handlerObjs)

            let myMapM :: (a -> IO (Maybe Word32)) -> [a] -> IO Word32
                myMapM _ [] = error "exception: no handler found (TODO2)"
                myMapM g (x:xs) = do
                  r <- g x
                  case r of
                    Just y -> return y
                    Nothing -> myMapM g xs
            let f :: (B.ByteString, Word32) -> IO (Maybe Word32)
                f (x, y) = do
                      printfEx $ printf "looking at @ %s\n" (show x)
                      x' <- isInstanceOf weax x
                      return $ if x' then Just y else Nothing
            -- by using myMapM, we avoid to look at *every* handler, but abort
            -- on the first match (yes, it's rather ugly with IO :/ better
            -- solutions are welcome)
            handlerNPC <- liftIO $ myMapM f handlerObjs
            liftIO $ printfEx $ printf "handler at: 0x%08x\n" handlerNPC
            emitSigIllTrap 2
            return $ fromIntegral handlerNPC
      return $ Just (trapaddr, ThrowException patcher)

    emit' insn = emit insn >> return Nothing

    emit :: J.Instruction -> CodeGen e s ()
    emit POP = add esp (ptrSize :: Word32) -- drop value
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
      add eax (ptrSize :: Word32) -- for "length" entry
      -- push amount of bytes to allocate
      push eax
      callMalloc
      pop eax -- ref to arraymemory
      pop ebx -- length
      mov (Disp 0, eax) ebx -- store length at offset 0
      push eax -- push ref again

    emit (CHECKCAST _) = nop -- TODO(bernhard): ...
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
    emit (ILOAD_ x) = emit (ILOAD $ cArgs_ x)
    emit (ALOAD x) = emit (ILOAD x)
    emit (ILOAD x) = push (Disp (cArgs x), ebp)

    emit (ASTORE_ x) = emit (ISTORE_ x)
    emit (ISTORE_ x) = emit (ISTORE $ cArgs_ x)
    emit (ASTORE x) = emit (ISTORE x)
    emit (ISTORE x) = do
      pop eax
      mov (Disp (cArgs x), ebp) eax

    emit (LDC1 x) = emit (LDC2 $ fromIntegral x)
    emit (LDC2 x) = do
      value <- case constsPool cls M.! x of
                    (CString s) -> liftIO $ getUniqueStringAddr s
                    (CInteger i) -> liftIO $ return i
                    e -> error $ "LDCI... missing impl.: " ++ show e
      push value

    emit IADD = do pop ebx; pop eax; add eax ebx; push eax
    emit ISUB = do pop ebx; pop eax; sub eax ebx; push eax
    emit IMUL = do pop ebx; pop eax; mul ebx; push eax
    emit IDIV = do pop ebx; pop eax; xor edx edx; div ebx; push eax
    emit IREM = do pop ebx; pop eax; xor edx edx; div ebx; push edx
    emit IXOR = do pop ebx; pop eax; xor eax ebx; push eax
    emit IUSHR = do pop ecx; pop eax; sar eax cl; push eax
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

    emit RETURN = do mov esp ebp; pop ebp; pop ebp; ret
    emit ARETURN = emit IRETURN
    emit IRETURN = do pop eax; emit RETURN
    emit invalid = error $ "insn not implemented yet: " ++ show invalid

    emitIF :: CMP -> CodeGen e s ()
    emitIF cond = let
      sid = case successor bb of TwoTarget _ t -> t; _ -> error "bad"
      l = getLabel sid lmap
      sid2 = case successor bb of TwoTarget t _ -> t; _ -> error "bad"
      l2 = getLabel sid2 lmap
      in do
        case cond of
          C_EQ -> je  l; C_NE -> jne l
          C_LT -> jl  l; C_GT -> jg  l
          C_GE -> jge l; C_LE -> jle l
        -- TODO(bernhard): ugly workaround, to get broken emitBB working
        --  (it didn't work for gnu/classpath/SystemProperties.java)
        jmp l2

    emitSigIllTrap :: Int -> CodeGen e s NativeWord
    emitSigIllTrap traplen = do
      when (traplen < 2) (error "emitSigIllTrap: trap len too short")
      trapaddr <- getCurrentOffset
      -- 0xffff causes SIGILL
      emit8 (0xff :: Word8); emit8 (0xff :: Word8)
      -- fill rest up with NOPs
      sequence_ [nop | _ <- [1 .. (traplen - 2)]]
      return trapaddr


  -- for locals we use a different storage
  cArgs :: Word8 -> Word32
  cArgs x = ptrSize * (argcount - x' + isLocal)
    where
      x' = fromIntegral x
      argcount = rawArgCount method
      isLocal = if x' >= argcount then (-1) else 2

  cArgs_ :: IMM -> Word8
  cArgs_ x = case x of I0 -> 0; I1 -> 1; I2 -> 2; I3 -> 3


  -- sign extension from w8 to w32 (over s8)
  --   unfortunately, hs-java is using Word8 everywhere (while
  --   it should be Int8 actually)
  s8_w32 :: Word8 -> Word32
  s8_w32 w8 = fromIntegral s8
    where s8 = fromIntegral w8 :: Int8

callMalloc :: CodeGen e s ()
callMalloc = do
  call mallocObjectAddr
  add esp (ptrSize :: Word32)
  push eax


-- harpy tries to cut immediates (or displacements), if they fit in 8bit.
-- however, this is bad for patching so we want to put always 32bit.

-- push imm32
push32 :: Word32 -> CodeGen e s ()
push32 imm32 = emit8 0x68 >> emit32 imm32

-- call disp32(%eax)
call32Eax :: Disp -> CodeGen e s ()
call32Eax (Disp disp32) = emit8 0xff >> emit8 0x90 >> emit32 disp32

-- push disp32(%eax)
push32RelEax :: Disp -> CodeGen e s ()
push32RelEax (Disp disp32) = emit8 0xff >> emit8 0xb0 >> emit32 disp32

-- mov %ebx, disp32(%eax)
mov32RelEbxEax :: Disp -> CodeGen e s ()
mov32RelEbxEax (Disp disp32) = emit8 0x89 >> emit8 0x98 >> emit32 disp32
