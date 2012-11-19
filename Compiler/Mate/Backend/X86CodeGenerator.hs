{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Compiler.Mate.Backend.X86CodeGenerator
  ( compileLinear
  , handleExceptionPatcher
  , compileStateInit
  ) where

import Prelude hiding (and, div, or)

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.IntervalMap as IM
import qualified Data.ByteString.Lazy as B
import Data.Int
import Data.Word
import Data.Maybe
import Data.List hiding (and, or)
import Data.Ord
import Data.Binary.IEEE754
import qualified Data.Bits as DB

import Control.Applicative hiding ((<*>))
import Control.Monad

import Foreign hiding (xor)
import Foreign.C.Types

import JVM.Assembler hiding (Instruction)
import JVM.ClassFile hiding (methodName)
import Data.Binary
import Data.BinaryState

import Harpy hiding (fst)
import Harpy.X86Disassembler

import qualified Compiler.Hoopl as H
import Compiler.Hoopl hiding (Label)
import Compiler.Mate.Frontend hiding (ptrSize, classf)
import Compiler.Mate.Backend.NativeSizes
import Compiler.Mate.Runtime.ClassHierarchy
import Compiler.Mate.Runtime.JavaObjects
import Compiler.Mate.Runtime.ClassPool
import Compiler.Mate.Runtime.MethodPool

import Compiler.Mate.Debug
import Compiler.Mate.Types
import Compiler.Mate.Utilities


foreign import ccall "&mallocObjectGC_stackstrace"
  mallocObjectAddr :: FunPtr (CPtrdiff -> CPtrdiff -> Int -> IO CPtrdiff)


data CompileState = CompileState
  { floatConsts :: M.Map Label Float
  , traps :: TrapMap
  , classf :: Class Direct
  , tryBlock :: Maybe (Word32, [(B.ByteString, Word32)])
  , exHandler :: M.Map Word32 Word32 {- JVM PC -> x86 EIP -}
  , exTable :: ExceptionMap Word32
  , gcpoints :: GCPoints
  , methodName :: B.ByteString }

compileStateInit :: Class Direct -> B.ByteString -> CompileState
compileStateInit cls m = CompileState
    { floatConsts = M.empty
    , traps = M.empty
    , classf = cls
    , tryBlock = Nothing
    , exHandler = M.empty
    , exTable = IM.empty
    , gcpoints = M.empty
    , methodName = m }

modifyState :: (CompileState -> CompileState) -> CodeGen e CompileState ()
modifyState f = do
  s <- getState
  setState (f s)


compileLinear :: M.Map Int32 H.Label -> [LinearIns HVarX86] -> Word32
              -> CodeGen e CompileState ([Instruction], NativeWord, TrapMap)
compileLinear lbls linsn stackAlloc = do
  -- entry sequence
  push ebp
  patchExPush <- (+1) <$> getCodeOffset
  push (0x13371337 :: Word32) -- (compile-time) patch it later manually
  mov ebp esp
  sub esp stackAlloc
  bblabels <- forM (M.elems lbls) $ \h -> do
                l <- newNamedLabel ("Label: " ++ show h)
                return (h, l)
  let lmap :: M.Map H.Label Label
      lmap = M.fromList bblabels
  let retseq = do mov esp ebp
                  pop ebp {- kill exmap ref on stack -}
                  pop ebp
                  ret
  let compileIns (Fst (IRLabel _ hlabel hmap maybeHandler)) = do
        defineLabel $ lmap M.! hlabel
        reip <- getCurrentOffset
        -- liftIO $ printf "maybeHandler: %s\n" (show maybeHandler)
        -- liftIO $ printf "hmap: %s\n\n" (show hmap)
        case maybeHandler of
          Nothing -> return ()
          Just jvmpc -> do
            modifyState (\s -> s {exHandler = M.insert jvmpc reip (exHandler s)})
        case hmap of
          [] -> return ()
          table -> modifyState (\s -> s {tryBlock = Just (reip, table)})
      compileIns (Mid ins) = girEmitOO ins
      compileIns (Lst ins) = do
        case ins of
          -- TODO: signed values
          IRIfElse _ jcmp src1 src2 h1 h2 -> do
            let l1 = lmap M.! h1
            let l2 = lmap M.! h2
            case (src1, src2) of -- attention: swap args
              (HIReg s1, HIReg s2) -> do
                cmp s2 s1
              (SpillIReg d1, HIReg s2) -> do
                cmp s2 (d1, ebp)
              (HIReg s1, SpillIReg d2) -> do
                mov eax (d2, ebp)
                cmp eax s1
              (SpillIReg d1, SpillIReg d2) -> do
                mov eax (d2, ebp)
                cmp eax (d1, ebp)
              (HIConstant c, HIReg s1) -> do
                cmp s1 (i32Tow32 c)
              (HIReg s1, HIConstant c) -> do
                mov eax (i32Tow32 c)
                cmp eax s1
              (SpillIReg d1, HIConstant c) -> do
                mov eax (i32Tow32 c)
                cmp eax (d1, ebp)
              (HIConstant c, SpillIReg s1) -> do
                cmp (s1, ebp) (i32Tow32 c)
              x -> error $ "IRifelse: not impl. yet" ++ show x
            case jcmp of
              C_EQ -> je  l1; C_NE -> jne l1
              C_LT -> jl  l1; C_GT -> jg  l1
              C_GE -> jge l1; C_LE -> jle l1
            jmp l2
          IRJump h -> jmp (lmap M.! h)
          IRExHandler _ -> error $ "emit: IRExHandlers: should not happen"
          IRSwitch _ src table -> do
            r2r eax src
            forM_ table $ \x -> case x of
                (Just val, label) -> do
                  cmp eax (i32Tow32 val)
                  je (lmap M.! label)
                (Nothing, label) -> do
                  jmp (lmap M.! label)
          IRReturn _ Nothing -> retseq
          IRReturn _ (Just (HFReg r)) -> do
            movss xmm7 r
            retseq
          IRReturn _ (Just retreg) -> do
            r2r eax retreg
            retseq
        st <- getState
        case tryBlock st of
          Nothing -> return ()
          Just (start_ip, hmap) -> do
            end_ip <- getCurrentOffset
            -- TODO: end_ip - 1?
            let key = IM.ClosedInterval start_ip end_ip
            case IM.lookup key (exTable st) of
              Nothing -> do
                let newmap = IM.insert (IM.ClosedInterval start_ip end_ip) hmap
                modifyState (\s -> s { exTable = newmap (exTable s) })
              Just hmap_old -> do
                let newmap = IM.insert (IM.ClosedInterval start_ip end_ip) (hmap ++ hmap_old)
                modifyState (\s -> s { exTable = newmap (exTable s) })
        modifyState (\s -> s { tryBlock = Nothing })
  forM_ linsn $ \ins -> do
    newNamedLabel ("ir: " ++ show ins) >>= defineLabel
    compileIns ins
  exTab <- exTable <$> getState
  -- liftIO $ printf "exTab: %s\n" (show exTab)
  exHandls <- exHandler <$> getState
  -- liftIO $ printf "exHandls: %s\n" (show exHandls)
  let exmap :: ExceptionMap Word32
      exmap = foldl'
              (\db key -> IM.update
                          (\entries ->
                            Just $
                             map (\(cn, x) -> (cn, exHandls M.! x)) entries
                          ) key db
              )
              exTab
              (IM.keys exTab)
  mname <- methodName <$> getState
  gcpts <- gcpoints <$> getState
  liftIO $ printfJit "gcpoints:\n"
  liftIO $ forM_ (M.toList gcpts) $ \(ip, lst) -> do
            printfJit $ printf "\teip: %08x\n" ip
            forM_ lst $ \x -> do
              printfJit $ printf "\t\t0x%08x\n" x
  let rsi = RuntimeStackInfo
             { rsiMethodname = mname
             , rsiExceptionMap = exmap
             , rsiGCPoints = gcpts
             }
  sptr_rsi <- liftIO $
    (fromIntegral . ptrToIntPtr . castStablePtrToPtr) <$> newStablePtr rsi
  emit32At patchExPush sptr_rsi
  floatconstants <- M.toList <$> floatConsts <$> getState
  forM_ floatconstants $ \(l, f) -> do
    defineLabel l
    emit32 (floatToWord f)
  nop; nop; nop; nop -- just some NOPs to fix up the disasm
  d <- if mateDEBUG then disassemble else return []
  ep <- fromIntegral <$> ptrToIntPtr <$> getEntryPoint
  tm <- traps <$> getState
  return (d, ep, tm)

select :: forall a b e s.
          (Sub a b, And a b, Add a b, Or a b, Xor a b) =>
          OpType -> a -> b -> CodeGen e s ()
select Add = add
select Sub = sub
select And = and
select Or = or
select Xor = xor
select x = error $ "codegen: select: not impl.: " ++ show x

girEmitOO :: MateIR HVarX86 O O -> CodeGen e CompileState ()
girEmitOO (IROp _ operation dst' src1' src2') =
    case operation of
      Mul -> girMul
      Div -> girDiv eax -- result is in eax
      Rem -> girDiv edx -- result is in edx
      ShiftRightLogical -> girShift shr
      ShiftRightArth -> girShift sar
      ShiftLeft -> girShift sal
      _ -> case (operation, dst', src1', src2') of
        -- handle special add cases (move instructions)
        (Add, HIReg dst, HIReg src1, HIConstant 0) -> mov dst src1
        (Add, SpillIReg d, SpillIReg s1, HIConstant 0) -> do
          mov eax (s1, ebp)
          mov (d, ebp) eax
        (Add, SpillIReg d, HIReg r1, HIConstant 0) -> mov (d, ebp) r1
        (Add, HIReg dst, SpillIReg r1, HIConstant 0) -> mov dst (r1, ebp)
        (Add, HIReg dst, HIConstant c1, HIConstant 0) -> mov dst (i32Tow32 c1)
        (Add, SpillIReg d, HIConstant c1, HIConstant 0) ->
          mov (d, ebp) (i32Tow32 c1)
        -- general case
        _ -> ge (select operation) dst' src1' src2'
  where
    ge :: (forall a b. (Sub a b, And a b, Add a b, Or a b, Xor a b)
                       => a -> b -> CodeGen e s ())
          -> HVarX86 -> HVarX86 -> HVarX86 -> CodeGen e s ()
    ge _ (SpillIReg d) (HIConstant c1) (HIConstant c2) = do
      let res = i32Tow32 $ case operation of
                  Add -> c1 + c2
                  And -> c1 .&. c2
                  Or -> c1 .|. c2
                  Xor -> c1 `DB.xor` c2
                  Sub -> error "emit: ge: opx: not sure if c1 - c2 or c2 - c1"
                  y -> error $ "emit: ge: opx: constant: " ++ show y
      mov (d, ebp) res
    ge opx dst src1 src2 = do
      r2r eax src2
      case src1 of
        HIConstant c -> opx eax (i32Tow32 c)
        HIReg r ->      opx eax r
        SpillIReg d ->  opx eax (d, ebp)
        y -> error $ "emit: ge: opx: src1: " ++ show y
      r2r dst eax

    girMul = do
      -- edx is killed by `mul' instruction
      freeRegFor edx dst' $ do
        gm dst' src1' src2'
    gm dst src1 src2
        | hvarIsConst src2 = gm' dst src2 src1
        | otherwise = gm' dst src1 src2
    gm' dst src1 src2 = do
      r2r eax src1
      case src2 of
        HIReg r -> mul r
        SpillIReg d -> mul (d, ebp)
        y -> error $ "emit: gm: src: " ++ show y
      r2r dst eax

    girShift :: (forall a b. (Shr a b, Sar a b, Sal a b)
                          => a -> b -> CodeGen e s ())
             -> CodeGen e s ()
    girShift shiftop = do
      freeRegFor ecx dst' $ do
        gs shiftop dst' src1' src2'

    gs :: (forall a b. (Shr a b, Sar a b, Sal a b)
                    => a -> b -> CodeGen e s ())
          -> HVarX86 -> HVarX86 -> HVarX86 -> CodeGen e s ()
    gs so dst src1 src2 = do
      r2r eax src2
      r2r ecx src1
      so eax cl
      r2r dst eax

    girDiv resreg = do -- `div' destroys eax and edx
      freeRegFor edx dst' $ do
        freeRegFor ebx dst' $ do
          r2r eax src2'
          r2r ebx src1'

          -- guard for exception
          lokay <- newNamedLabel "lokay"
          cmp ebx (0 :: Word32)
          jne lokay
          -- if null, then create exception-object in signal handler
          trapaddr <- emitSigIllTrap 2
          let patcher wbr = do
                emitSigIllTrap 2
                liftIO $ do
                  ex <- allocAndInitObject "java/lang/ArithmeticException"
                  handleExceptionPatcher (wbr { wbEax = ex })
          modifyState (\s -> s { traps = M.insert trapaddr (ThrowException patcher) (traps s) })
          lokay @@ xor edx edx
          div ebx
          -- move result (depending on the operation) into destination
          r2r dst' resreg

girEmitOO (IRInvoke _ (RTPoolCall cpidx mapping) haveReturn ct) = do
  let static = girStatic cpidx haveReturn ct mapping
  let virtual = girVirtual cpidx haveReturn ct mapping
  case ct of
    CallStatic -> static
    CallSpecial -> static
    CallVirtual -> virtual
    CallInterface -> virtual
girEmitOO (IRLoad _ (RTPoolCall x mapping) (HIConstant 0) dst) = do
  cls <- classf <$> getState
  case constsPool cls M.! x of
    (CClass objname) -> do -- `new' object
      saveRegs
      trapaddr <- emitSigIllTrap 5
      callMallocGCPoint mapping
      restoreRegs
      -- 0x13371337 is just a placeholder; will be replaced with mtable ptr
      mov (Disp 0, eax) (0x13371337 :: Word32)
      mov (Disp 4, eax) (0 :: Word32)
      r2r dst eax
      let patcher wbr = do
            objsize <- liftIO $ getObjectSize objname
            push32 objsize
            callMalloc
            restoreRegs
            mtable <- liftIO $ getMethodTable objname
            mov (Disp 0, eax) mtable
            mov (Disp 4, eax) (0 :: Word32)
            r2r dst eax
            return wbr
      s <- getState
      setState (s { traps = M.insert trapaddr (NewObject patcher) (traps s) })
    e -> error $ "emit irload: missing impl.: " ++ show e

girEmitOO (IRLoad _ (RTPool x) (HIConstant 0) dst) = do
  cls <- classf <$> getState
  case constsPool cls M.! x of
    (CString s) -> do -- load str (ldc)
      sref <- liftIO $ getUniqueStringAddr s
      r2r dst sref
    (CInteger i) -> do -- load integer (ldc)
      r2r dst i
    (CField rc fnt) -> do -- getstatic
      let sfi = StaticField $ StaticFieldInfo rc (ntName fnt)
      trapaddr <- getCurrentOffset
      mov eax (Addr 0)
      r2r dst eax
      s <- getState
      setState (s { traps = M.insert trapaddr sfi (traps s) })
    e -> error $ "emit: irload2: missing impl.: " ++ show e

girEmitOO (IRLoad _ (RTPool x) src dst) = do
  cls <- classf <$> getState
  case constsPool cls M.! x of
    (CField rc fnt) -> do -- getfield
      freeRegFor ebx dst $ do
        r2r eax src
        trapaddr <- emitSigIllTrap 7
        let patcher wbr = do
              offset <- liftIO $ fromIntegral <$> getFieldOffset rc (ntName fnt)
              mov ebx (Disp offset, eax)
              return wbr
        r2r dst ebx
        let ofp = ObjectField patcher
        s <- getState
        setState (s { traps = M.insert trapaddr ofp (traps s) })
    y -> error $ "emit: irload: missing impl.: getfield or something: " ++ show y

girEmitOO (IRLoad _ (RTArray ta objType regmapping arrlen) (HIConstant 0) dst) = do
  let tsize = case decodeS (0 :: Integer) (B.pack [ta]) of
                T_INT -> 4
                T_CHAR -> 4
                _ -> error "newarray: type not implemented yet"
  let len = arrlen * tsize
  saveRegs
  push (len + (3 * ptrSize))
  callMallocGCPoint regmapping
  restoreRegs
  case objType of
    PrimitiveType -> mov (Disp 0, eax) (0x1228babe :: Word32)
    ReferenceType -> mov (Disp 0, eax) (0x1227babe :: Word32)
  mov (Disp 4, eax) (0x1337babe :: Word32) -- gcinfo
  mov (Disp 8, eax) arrlen -- store length at offset 0
  r2r dst eax

girEmitOO (IRLoad _ RTNone src dst) = do -- arraylength
  r2r eax src
  mov eax (Disp 8, eax)
  r2r dst eax

girEmitOO (IRLoad _ (RTIndex idx typ) src dst) = do
    freeRegFor ebx dst $ do
      case idx of
        HIConstant i -> mov eax $ ((i32Tow32 i) * (typeSize typ)) + 0xc
        _ -> do
          case idx of
            HIReg i -> mov eax i
            SpillIReg d -> mov eax (d, ebp)
            y -> error $ "girEmitOO: irload: rtindex: idx: " ++ show y
          mov ebx (typeSize typ :: Word32)
          mul ebx
          add eax (0xc :: Word32)
      case src of
        HIReg s -> if s == ebx then add eax (Disp 0, esp) else add eax s
        SpillIReg d -> do add eax (d, ebp)
        y -> error $ "girEmitOO: irload: rtindex: src: " ++ show y
      case dst of
        HIReg d -> do mov d (Disp 0, eax)
        SpillIReg d -> do
          mov ebx (Disp 0, eax)
          mov (d, ebp) ebx
        y -> error $ "girEmitOO: irload: rtindex: dst: " ++ show y

girEmitOO (IRStore _ (RTPool x) obj src) = do
  cls <- classf <$> getState
  case constsPool cls M.! x of
    (CField rc fnt) -> do
      if obj == HIConstant 0
        then do -- putstatic
          let sfi = StaticField $ StaticFieldInfo rc (ntName fnt)
          r2r eax src
          trapaddr <- getCurrentOffset
          mov (Addr 0) eax
          s <- getState
          setState (s { traps = M.insert trapaddr sfi (traps s) })
        else do -- putfield
          push ebx
          r2r eax obj
          r2r ebx src
          -- like: 89 98 77 66 37 13       mov    %ebx,0x13376677(%eax)
          trapaddr <- emitSigIllTrap 6
          let patcher wbr = do
                let (cname, fname) = buildFieldOffset cls x
                offset <- liftIO $ fromIntegral <$> getFieldOffset cname fname
                mov32RelEbxEax (Disp offset) -- set field
                return wbr
          pop ebx
          s <- getState
          setState (s { traps = M.insert trapaddr (ObjectField patcher) (traps s)})
    e -> error $ "emit: irstore: missing impl.: " ++ show e
girEmitOO (IRStore _ (RTIndex idx typ) dst src) = do
    freeRegFor ebx dst $ do
      case idx of
        HIConstant _ -> mov eax (0 :: Word32)
        _ -> do
          case idx of
            HIReg i -> mov eax i
            SpillIReg d -> mov eax (d, ebp)
            y -> error $ "emit: irstore: rtindex: idx: " ++ show y
          mov ebx (typeSize typ :: Word32)
          mul ebx
          add eax (0xc :: Word32)
      case dst of
        HIReg d -> if d == ebx then add eax (Disp 0, esp) else add eax d
        SpillIReg d -> add eax (d, ebp)
        y -> error $ "girEmitOO: irstore: rtindex: dst: " ++ show y
      -- store array elem
      r2r ebx src
      case idx of
        HIConstant i -> mov (Disp ((+0xc) . (*(typeSize typ)) $ i32Tow32 i), eax) ebx
        _ -> mov (Disp 0, eax) ebx

girEmitOO ins@(IRStore _ _ _ _) = do
  error $ "irstore: emit: " ++ show ins
girEmitOO (IRPush _ _ (HIReg x)) = push x
girEmitOO (IRPush _ _ (HIConstant x)) = push (i32Tow32 x)
girEmitOO (IRPush _ _ (SpillIReg d)) = push (d, ebp)
girEmitOO (IRPrep SaveRegs regs) = do
  forM_ regs $ \(HIReg x, _) -> do
    mov (Disp (fromJust (saveReg x)), ebp) x
girEmitOO (IRPrep RestoreRegs regs) = do
  forM_ regs $ \(HIReg x, _) -> do
    mov x (Disp (fromJust (saveReg x)), ebp)
girEmitOO (IRMisc1 _ jins vreg) = do
  case jins of
    ATHROW -> do
      r2r eax vreg
      trapaddr <- emitSigIllTrap 2
      let patcher wbr = do
            emitSigIllTrap 2
            liftIO $ handleExceptionPatcher wbr
      s <- getState
      setState (s { traps = M.insert trapaddr (ThrowException patcher) (traps s) })
    CHECKCAST _ -> do
      nop -- TODO ..
    x -> error $ "emit: misc1: " ++ show x
girEmitOO (IRMisc2 _ jins dst src) = do
  case jins of
    INSTANCEOF cpidx -> do
      cls <- classf <$> getState
      let movres :: Word32 -> CodeGen e s ()
          movres r = r2r dst r
      r2r eax src
      -- place something like `mov edx $mtable_of_objref' instead
      trapaddr <- emitSigIllTrap 4
      movres 0
      let patcher wbr = do
            emitSigIllTrap 4
            let classname = buildClassID cls cpidx
            check <- liftIO $ isInstanceOf (fromIntegral $ wbEax wbr) classname
            if check
              then movres 1
              else movres 0
            return $ wbr {wbEip = wbEip wbr + 4}
      s <- getState
      setState (s { traps = M.insert trapaddr (InstanceOf patcher) (traps s) })
    x -> error $ "emit: misc2: " ++ show x
girEmitOO x = error $ "girEmitOO: insn not implemented: " ++ show x

girStatic :: Word16 -> Maybe HVarX86 -> CallType -> PreGCPoint
          -> CodeGen e CompileState ()
girStatic cpidx haveReturn ct mapping = do
  cls <- classf <$> getState
  let hasThis = ct == CallSpecial
  let l = buildMethodID cls cpidx
  newNamedLabel (show l) >>= defineLabel
  -- like: call $0x01234567
  calladdr <- emitSigIllTrap 5
  let patcher wbr = do
        entryAddr <- liftIO $ lookupMethodEntry l
        call (fromIntegral (entryAddr - (wbEip wbr + 5)) :: NativeWord)
        return wbr
  setGCPoint mapping
  -- discard arguments on stack
  let argcnt = ((if hasThis then 1 else 0)
               + methodGetArgsCount (methodNameTypeByIdx cls cpidx)
               ) * ptrSize
  when (argcnt > 0) (add esp argcnt)

  case haveReturn of
    Just (HIReg dst) -> mov dst eax
    Just y -> error $ "girStatic: haveReturn: " ++ show y
    Nothing -> return ()
  s <- getState
  setState (s { traps = M.insert calladdr (StaticMethod patcher) (traps s) })

girVirtual :: Word16 -> Maybe HVarX86 -> CallType -> PreGCPoint
           -> CodeGen e CompileState ()
girVirtual cpidx haveReturn ct mapping = do
  let isInterface = ct == CallInterface
  cls <- classf <$> getState
  let mi@(MethodInfo methodname objname msig@(MethodSignature _ _)) =
          buildMethodID cls cpidx
  newNamedLabel (show mi) >>= defineLabel
  -- get method offset for call @ runtime
  let offset =
        if isInterface
          then getInterfaceMethodOffset objname methodname (encode msig)
          else getMethodOffset objname (methodname `B.append` encode msig)
  -- objref lives somewhere on the argument stack
  mov ebx (Disp 0, esp)
  when isInterface $
    mov ebx (Disp 0, ebx) -- get method-table-ptr, keep it in ebx
  -- get method-table-ptr (or interface-table-ptr)
  mov eax (Disp 0, ebx)
  -- make actual (indirect) call
  calladdr <- getCurrentOffset
  -- will be patched to this: call (Disp 0xXXXXXXXX, eax)
  emitSigIllTrap 6
  setGCPoint mapping

  -- discard arguments on stack (`+1' for "this")
  let argcnt = ptrSize * (1 + methodGetArgsCount (methodNameTypeByIdx cls cpidx))
  when (argcnt > 0) (add esp argcnt)

  case haveReturn of
    Just (HIReg dst) -> mov dst eax
    Nothing -> return ()
    Just y -> error $ "girVirtual: haveReturn: " ++ show y
  -- note, that "mi" has the wrong class reference here.
  -- we figure that out at run-time, in the methodpool,
  -- depending on the method-table-ptr
  s <- getState
  setState (s { traps = M.insert calladdr
                        (VirtualCall isInterface mi offset)
                        (traps s) })

setGCPoint :: [(HVarX86, VarType)] -> CodeGen e CompileState ()
setGCPoint mapping = do
  ip <- getCurrentOffset
  -- liftIO $ printfJit "setGCPoint: unfiltered:\n"
  -- liftIO $ forM_ mapping $ \x -> do
  --               printfJit $ printf "\t%s\n" (show x)
  let filtered = filterJRefs mapping
  -- liftIO $ printfJit "setGCPoint: filtered:\n"
  -- liftIO $ forM_ filtered $ \x -> do
  --               printfJit $ printf "\t0x%08x\n" x
  s <- getState
  setState (s { gcpoints = M.insert ip filtered (gcpoints s) })


filterJRefs :: [(HVarX86, VarType)] -> GCPoint
filterJRefs = mapMaybe frefs
  where
    frefs (SpillIReg (Disp d), JRef) = Just d
    frefs (HIReg reg32, JRef) = saveReg reg32
    frefs _ = Nothing

freeRegFor :: Reg32 -> HVarX86 -> CodeGen e s r -> CodeGen e s r
freeRegFor r32 dst body = do
  let isNotDst =
        case dst of
          HIReg dst' -> dst' /= r32
          _ -> True
  when isNotDst $ push r32
  res <- body
  when isNotDst $ pop r32
  return res

-- transfer between "HVar" and real maschine registers
class RegisterToHvar a b where
  r2r :: a -> b -> CodeGen e s ()

instance RegisterToHvar HVarX86 Word32 where
  r2r (HIReg reg) src = mov reg src
  r2r (SpillIReg disp) src = mov (disp, ebp) src
  r2r i _ = error $ "r2r HVarX86 Word32: " ++ show i

instance RegisterToHvar HVarX86 Reg32 where
  r2r (HIReg reg) src = mov reg src
  r2r (SpillIReg disp) src = mov (disp, ebp) src
  r2r i _ = error $ "r2r HVarX86 Reg32: " ++ show i

instance RegisterToHvar Reg32 HVarX86 where
  r2r dst (HIReg reg) = mov dst reg
  r2r dst (SpillIReg disp) = mov dst (disp, ebp)
  r2r dst (HIConstant i32) = mov dst (i32Tow32 i32)
  r2r _ not_yet = error $ "r2r Reg32 HVarX86: not impl. yet: " ++ show not_yet

hvarIsConst :: HVarX86 -> Bool
hvarIsConst (HIConstant _) = True
hvarIsConst (HFConstant _) = True
hvarIsConst _ = False

saveReg :: Reg32 -> Maybe Word32
saveReg (Reg32 w8) =
  case w8 of
    0 {- eax -} -> Nothing
    1 {- ecx -} -> Just 0xfffffffc
    2 {- edx -} -> Just 0xfffffff8
    3 {- ebx -} -> Just 0xfffffff4
    4 {- esp -} -> error "saveReg: esp???"
    5 {- ebp -} -> error "saveReg: ebp???"
    6 {- esi -} -> Just 0xfffffff0
    7 {- edi -} -> Just 0xffffffec
    _ -> error "saveReg: ?????"

saveRegs :: CodeGen e s ()
saveRegs = do
  forM_ [ecx, edx, ebx, esi, edi] $ \x -> do
    mov (Disp (fromJust (saveReg x)), ebp) x

restoreRegs :: CodeGen e s ()
restoreRegs = do
  forM_ [ecx, edx, ebx, esi, edi] $ \x -> do
    mov x (Disp (fromJust (saveReg x)), ebp)

-- helper
callMalloc :: CodeGen e s ()
callMalloc = do
  push ebp
  push esp
  call mallocObjectAddr
  add esp ((3 * ptrSize) :: Word32)

callMallocGCPoint :: PreGCPoint -> CodeGen e CompileState ()
callMallocGCPoint regmapping = do
  push ebp
  push esp
  call mallocObjectAddr
  setGCPoint regmapping
  add esp ((3 * ptrSize) :: Word32)


-- harpy tries to cut immediates (or displacements), if they fit in 8bit.
-- however, this is bad for patching so we want to put always 32bit.

-- push imm32
push32 :: Word32 -> CodeGen e s ()
push32 imm32 = emit8 0x68 >> emit32 imm32

-- mov %ebx, disp32(%eax)
mov32RelEbxEax :: Disp -> CodeGen e s ()
mov32RelEbxEax (Disp disp32) = emit8 0x89 >> emit8 0x98 >> emit32 disp32

emitSigIllTrap :: Int -> CodeGen e s NativeWord
emitSigIllTrap traplen = do
  when (traplen < 2) (error "emitSigIllTrap: trap len too short")
  trapaddr <- getCurrentOffset
  -- 0xffff causes SIGILL
  emit8 (0xff :: Word8); emit8 (0xff :: Word8)
  -- fill rest up with NOPs
  sequence_ [nop | _ <- [1 .. (traplen - 2)]]
  return trapaddr

getCurrentOffset :: CodeGen e s Word32
getCurrentOffset = do
  ep <- (fromIntegral . ptrToIntPtr) <$> getEntryPoint
  offset <- fromIntegral <$> getCodeOffset
  return $ ep + offset

typeSize :: Num a => VarType -> a
typeSize JInt = 4
typeSize JRef = 4
typeSize x = error $ "typeSize: " ++ show x

handleExceptionPatcher :: ExceptionHandler
handleExceptionPatcher wbr = do
  let weip = fromIntegral $ wbEip wbr
  printfEx $ printf "eip of throw: 0x%08x %d\n" weip weip
  handleException weip (wbEbp wbr) (wbEsp wbr)
    where
      weax = fromIntegral (wbEax wbr) :: Word32
      unwindStack :: CPtrdiff -> IO WriteBackRegs
      unwindStack rebp = do
        let nesp = rebp + 8
        -- get ebp of caller
        nebp <- peek (intPtrToPtr . fromIntegral $ (nesp - 4))
        printfEx $ printf "nebp: 0x%08x\n" (fromIntegral nebp :: Word32)
        printfEx $ printf "nesp: 0x%08x\n" (fromIntegral nesp :: Word32)
        -- get return addr
        neip <- peek . intPtrToPtr . fromIntegral $ nesp
        printfEx $ printf "neip: 0x%08x\n" (neip :: Word32)
        handleException neip nebp nesp
      handleException :: Word32 -> CPtrdiff -> CPtrdiff -> IO WriteBackRegs
      handleException weip rebp resp = do
        -- get full exception map from stack
        stblptr <- peek (intPtrToPtr . fromIntegral $ rebp) :: IO Word32
        let sptr = castPtrToStablePtr $ intPtrToPtr $ fromIntegral stblptr
        stackinfo <- deRefStablePtr sptr :: IO RuntimeStackInfo
        let exmap = rsiExceptionMap stackinfo
        printfEx $ printf "methodname: %s\n" (toString $ rsiMethodname stackinfo)
        printfEx $ printf "size: %d\n" (IM.size exmap)
        printfEx $ printf "exmap: %s\n" (show exmap)

        -- find the handler in a region. if there isn't a proper
        -- handler, go to the caller method (i.e. unwind the stack)
        let searchRegion :: [IM.Interval Word32] -> IO WriteBackRegs
            searchRegion [] = do
              printfEx "unwind stack now. good luck(x)\n\n"
              unwindStack rebp
            searchRegion (r:rs) = do
              -- let's see if there's a proper handler in this range
              res <- findHandler r exmap
              case res of
                Just x -> return x
                Nothing -> searchRegion rs
        searchRegion . map fst $ exmap `IM.containing` weip
          where
            findHandler :: IM.Interval Word32 -> ExceptionMap Word32 -> IO (Maybe WriteBackRegs)
            findHandler key exmap = do
              printfEx $ printf "key is: %s\n" (show key)
              -- reverse the list to get the innermost handler (see
              -- ./tests/Exception11.java )
              let handlerObjs = reverse
                              $ sortBy (comparing snd)
                              $ exmap IM.! key
              printfEx $ printf "handlerObjs: %s\n" (show handlerObjs)

              let myMapM :: (a -> IO (Maybe Word32)) -> [a] -> IO (Maybe WriteBackRegs)
                  myMapM _ [] = return Nothing
                  myMapM g (x:xs) = do
                    r <- g x
                    case r of
                      Just y -> return $ Just WriteBackRegs
                                  { wbEip = fromIntegral y
                                  , wbEbp = rebp
                                  , wbEsp = resp
                                  , wbEax = fromIntegral weax }
                      Nothing -> myMapM g xs
              let f :: (B.ByteString, Word32) -> IO (Maybe Word32)
                  f (x, y) = do
                        printfEx $ printf "looking at @ %s\n" (show x)
                        -- on B.empty, it's the "generic handler"
                        -- (e.g. finally)
                        x' <- if x == B.empty then return True else isInstanceOf weax x
                        return $ if x' then Just y else Nothing
              -- by using myMapM, we avoid to look at *every* handler,
              -- but abort on the first match (yes, it's rather
              -- ugly :/ better solutions are welcome)
              myMapM f handlerObjs
