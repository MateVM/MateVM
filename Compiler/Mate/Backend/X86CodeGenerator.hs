{-# LANGUAGE GADTs #-}
module Compiler.Mate.Backend.X86CodeGenerator
  ( compileLinear
  ) where

import qualified Data.Map as M
import Data.Int
import Data.Word
import Data.Binary.IEEE754

import Control.Applicative hiding ((<*>))
import Control.Monad

import JVM.Assembler hiding (Instruction)

import Harpy
import Harpy.X86Disassembler

import qualified Compiler.Hoopl as H
import Compiler.Hoopl hiding (Label)
import Compiler.Mate.Utils
import Compiler.Mate.Frontend.IR
import Compiler.Mate.Frontend.Linear

type CompileState = M.Map Label Float

compileLinear :: M.Map Int32 H.Label -> [LinearIns HVar]
              -> CodeGen e CompileState [Instruction]
compileLinear lbls linsn = do
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
  floatconstants <- M.toList <$> getState
  forM_ floatconstants $ \(l, f) -> do
    defineLabel l
    emit32 (floatToWord f)
  forM_ [0..0x3] $ \_ -> nop -- just some NOPs to fix up the disasm
  disassemble

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
      setState (M.insert c f s)
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
girEmitOO (IRLoadRT x (HIReg dst)) = do
  newNamedLabel ("TODO: RT NEW: " ++ show x) >>= defineLabel
  mov dst (0 :: Word32)
girEmitOO (IRLoad (HIReg memsrc) (HIReg dst)) = do
  mov dst (Disp 0, memsrc)
girEmitOO (IRStore (HIReg memdst) (HIConstant c)) = do
  mov (Disp 0, memdst) (i32tow32 c)
girEmitOO (IRPush _ (HIReg x)) = push x
girEmitOO x = error $ "girEmitOO: insn not implemented: " ++ show x
