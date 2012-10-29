{-# LANGUAGE GADTs #-}
module Compiler.Mate.Backend.X86CodeGenerator
  ( compileLinear
  , handleExceptionPatcher
  , call32Eax
  , push32RelEax
  , mov32RelEbxEax
  , compileStateInit
  ) where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Data.Int
import Data.Word
import Data.List
import Data.Binary.IEEE754

import Control.Applicative hiding ((<*>))
import Control.Monad

import Foreign hiding (xor)
import Foreign.C.Types

import JVM.Assembler hiding (Instruction)
import JVM.ClassFile

import Harpy
import Harpy.X86Disassembler

import qualified Compiler.Hoopl as H
import Compiler.Hoopl hiding (Label)
import Compiler.Mate.Frontend hiding (ptrSize, classf)
import Compiler.Mate.Backend.NativeSizes
import Compiler.Mate.Runtime.ClassHierarchy
import Compiler.Mate.Runtime.JavaObjects

import Compiler.Mate.Debug
import Compiler.Mate.Types


foreign import ccall "&mallocObjectGC_stackstrace"
  mallocObjectAddr :: FunPtr (CPtrdiff -> CPtrdiff -> Int -> IO CPtrdiff)


compileStateInit :: Class Direct -> CompileState
compileStateInit cls = CompileState
    { floatConsts = M.empty
    , traps = M.empty
    , classf = cls }

data CompileState = CompileState
  { floatConsts :: M.Map Label Float
  , traps :: TrapMap
  , classf :: Class Direct }

i32tow32 :: Int32 -> Word32
i32tow32 = fromIntegral

compileLinear :: M.Map Int32 H.Label -> [LinearIns HVar]
              -> CodeGen e CompileState ([Instruction], NativeWord, TrapMap)
compileLinear lbls linsn = do
  ep <- fromIntegral <$> ptrToIntPtr <$> getEntryPoint
  bblabels <- forM (M.elems lbls) $ \h -> do
                l <- newNamedLabel ("Label: " ++ show h)
                return (h, l)
  let lmap :: M.Map H.Label Label
      lmap = M.fromList bblabels
  let compileIns (Fst (IRLabel h)) = defineLabel $ lmap M.! h
      compileIns (Mid ins) = girEmitOO ins
      compileIns (Lst ins) = case ins of
        IRIfElse jcmp src1 src2 h1 h2 -> do
          let l1 = lmap M.! h1
          let l2 = lmap M.! h2
          case (src1, src2) of
            (HIReg s1, HIReg s2) -> cmp s1 s2
            (HIConstant c, HIReg s1) -> cmp s1 (i32tow32 c)
            (HIReg s1, HIConstant c) -> cmp s1 (i32tow32 c)
            (HIConstant c, SpillIReg s1) -> do
              let se = (s1, ebp)
              cmp se (i32tow32 c) -- TODO: invert LE/GEresult??
            x -> error $ "IRifelse: not impl. yet" ++ show x
          case jcmp of
            C_EQ -> je  l1; C_NE -> jne l1
            C_LT -> jl  l1; C_GT -> jg  l1
            C_GE -> jge l1; C_LE -> jle l1
          jmp l2
        IRJump h -> jmp (lmap M.! h)
        IRReturn Nothing -> ret
        IRReturn (Just (HIReg r)) -> do mov eax r; ret
        IRReturn (Just (HIConstant c)) -> do mov eax (i32tow32 c); ret
        IRReturn (Just (SpillIReg d)) -> do
          let src = (d, ebp)
          mov eax src
          ret
        IRReturn (Just (HFReg r)) -> do
          movss xmm7 r
          ret
        IRReturn x -> error $ "IRReturn: impl. me: " ++ show x
  mapM_ compileIns linsn
  floatconstants <- M.toList <$> floatConsts <$> getState
  forM_ floatconstants $ \(l, f) -> do
    defineLabel l
    emit32 (floatToWord f)
  nop; nop; nop; nop -- just some NOPs to fix up the disasm
  d <- disassemble
  tm <- traps <$> getState
  return (d, ep, tm)

i322w32 :: Int32 -> Word32
i322w32 = fromIntegral

girEmitOO :: MateIR HVar O O -> CodeGen e CompileState ()
girEmitOO (IROp Add dst' src1' src2') =
    ge dst' src1' src2'
  where
    ge :: HVar -> HVar -> HVar -> CodeGen e CompileState ()
    ge (HIReg dst) (HIReg src1) (HIReg src2)
        | dst == src1 = add src1 src2
        | dst == src2 = add src2 src1
        | otherwise = do mov dst src1; add dst src2
    ge (HIReg dst) (HIConstant c1) (HIConstant c2) =
      mov dst (fromIntegral $ c1 + c2 :: Word32)

    ge (HIReg dst) (HIConstant c1) (HIReg src2) = do
      mov dst src2
      when (c1 /= 0) $ add dst (fromIntegral c1 :: Word32)
    ge (HIReg dst) (HIConstant c1) (SpillIReg disp) = do
      let src2 = (disp, ebp)
      mov dst src2
      when (c1 /= 0) $ add dst (fromIntegral c1 :: Word32)
    ge (HIReg dst) (SpillIReg disp) (HIReg src2) = do
      let src1 = (disp, ebp)
      mov dst src2
      add dst src1
    ge (HIReg dst) src1 c1@(HIConstant _) = ge (HIReg dst) c1 src1
    ge (HIReg dst) src1 spill@(SpillIReg _) = ge (HIReg dst) spill src1
    ge (HIReg dst) spill@(SpillIReg _) src2 = ge (HIReg dst) src2 spill
    ge (SpillIReg disp) (HIReg src1) (HIReg src2) = do
      let dst = (disp, ebp)
      mov dst src1
      add dst src2

    ge (HFReg dst) (HFReg src1) (HFReg src2) = do
      movss dst src2
      addss dst src1
    ge (HFReg dst) (HFConstant c1) (HFConstant c2) = do
      let f = c1 + c2
      c <- newNamedLabel ("float constant: " ++ show f)
      s <- getState
      setState (s { floatConsts = M.insert c f (floatConsts s)})
      movss dst c
    ge (HFReg dst) (HFReg src) (HFConstant 0) =
      movss dst src
    ge (HFReg dst) (HFReg src) (HIConstant 0) =
      movss dst src
    ge (SpillFReg d) c1@(HFConstant _) c2@(HFConstant _) = do
      let dst = (d, ebp)
      ge (HFReg xmm7) c1 c2
      movss dst xmm7
    ge (SpillFReg d) (HFReg src) (HFConstant 0) = do
      let dst = (d, ebp)
      movss dst src
    ge (HFReg dst) (SpillFReg d) (HFConstant 0) = do
      let src = (d, ebp)
      movss dst src
    ge p1 p2 p3 = error $ "girEmit (add): " ++ show p1 ++ ", " ++ show p2 ++ ", " ++ show p3
girEmitOO (IROp Sub dst' src1' src2') = do
    ge dst' src1' src2'
  where
    ge :: HVar -> HVar -> HVar -> CodeGen e s ()
    ge (HIReg dst) (HIReg src1) (HIReg src2) = do
      mov dst src2; sub dst src1
    ge (HIReg dst) (HIConstant i32) (HIReg src2) = do
      mov dst src2; sub dst (i322w32 i32)
    ge (HIReg dst) (HIConstant i32) (SpillIReg s2) = do
      let src2 = (s2, ebp)
      mov dst src2; sub dst (i322w32 i32)
    ge _ _ _ = error $ "sub: not impl.: " ++ show dst' ++ ", "
                     ++ show src1' ++ ", " ++ show src2'
girEmitOO (IROp Mul _ _ _) = do
  newNamedLabel "TODO! IROp Mul" >>= defineLabel
  nop
girEmitOO (IRInvoke _ _) = do
  newNamedLabel "TODO (call)" >>= defineLabel
  call (0x0 :: Word32)
girEmitOO (IRLoad (RTPool x) (HIConstant 0) (HIReg dst)) = do
  newNamedLabel ("TODO: RT `NEW' or `GESTATIC' or `LDC': " ++ show x) >>= defineLabel
  cls <- classf <$> getState
  case constsPool cls M.! x of
    (CString s) -> do
      sref <- liftIO $ getUniqueStringAddr s
      mov dst sref
    (CInteger i) -> do
      mov dst i
    (CField rc fnt) -> do
      let sfi = StaticField $ StaticFieldInfo rc (ntName fnt)
      trapaddr <- getCurrentOffset
      mov dst (0 :: Word32)
      s <- getState
      setState (s { traps = M.insert trapaddr sfi (traps s) })
    e -> error $ "emit: irload: missing impl.: " ++ show e
girEmitOO (IRLoad rt (HIReg memsrc) (HIReg dst)) = do
  error "irload: emit: use rt"
  mov dst (Disp 0, memsrc)
girEmitOO (IRStore rt (HIReg memdst) (HIConstant c)) = do
  error "irstore: emit: use rt"
  mov (Disp 0, memdst) (i32tow32 c)
girEmitOO (IRPush _ (HIReg x)) = push x
girEmitOO x = error $ "girEmitOO: insn not implemented: " ++ show x


-- helper
callMalloc :: CodeGen e s ()
callMalloc = do
  push ebp
  push esp
  call mallocObjectAddr
  add esp ((3 * ptrSize) :: Word32)
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

emitSigIllTrap :: Int -> CodeGen e s NativeWord
emitSigIllTrap traplen = do
  when (traplen < 2) (error "emitSigIllTrap: trap len too short")
  trapaddr <- getCurrentOffset
  -- 0xffff causes SIGILL
  emit8 (0xff :: Word8); emit8 (0xff :: Word8)
  -- fill rest up with NOPs
  sequence_ [nop | _ <- [1 .. (traplen - 2)]]
  return trapaddr
-- /helper

getCurrentOffset :: CodeGen e s Word32
getCurrentOffset = do
  ep <- (fromIntegral . ptrToIntPtr) <$> getEntryPoint
  offset <- fromIntegral <$> getCodeOffset
  return $ ep + offset

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
        printfEx $ printf "size: %d\n" (M.size exmap)
        printfEx $ printf "exmap: %s\n" (show $ M.toList exmap)

        -- find the handler in a region. if there isn't a proper
        -- handler, go to the caller method (i.e. unwind the stack)
        let searchRegion :: [(Word32, Word32)] -> IO WriteBackRegs
            searchRegion [] = do
              printfEx "unwind stack now. good luck(x)\n\n"
              unwindStack rebp
            searchRegion (r:rs) = do
              -- let's see if there's a proper handler in this range
              res <- findHandler r exmap
              case res of
                Just x -> return x
                Nothing -> searchRegion rs
        -- is the EIP somewhere in the range?
        let matchingIPs = filter (\(x, y) -> weip >= x && weip <= y)
        -- if `fst' is EQ, sort via `snd', but reverse
        let ipSorter (x1, y1) (x2, y2) =
              case x1 `compare` x2 of
                EQ -> case y1 `compare` y2 of
                        LT -> GT; GT -> LT; EQ -> EQ
                x -> x
        -- due to reversing the list, we get the innermost range at
        -- nested try/catch statements
        searchRegion . reverse . sortBy ipSorter . matchingIPs . M.keys $ exmap
          where
            findHandler :: (Word32, Word32) -> ExceptionMap Word32 -> IO (Maybe WriteBackRegs)
            findHandler key exmap = do
              printfEx $ printf "key is: %s\n" (show key)
              let handlerObjs = exmap M.! key
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
