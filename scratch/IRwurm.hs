{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B
import Data.Int
import Data.Word
import Data.Maybe
import Control.Applicative hiding ((<*>))

import Harpy
import Harpy.X86Disassembler

import JVM.Assembler hiding (Instruction)
import qualified JVM.Assembler as J
import JVM.ClassFile
import JVM.Converter

import Compiler.Hoopl hiding (Label)
import qualified Compiler.Hoopl as H

import Control.Monad.State

import Debug.Trace
import Text.Printf

{- TODO
(.) typeclass for codeemitting: http://pastebin.com/RZ9qR3k7 (depricated) || http://pastebin.com/BC3Jr5hG
(.) what about argument passing to methods
(.) what about return values in methods (related problem to above?)
(.) hoopl passes
-}

data MateIR t e x where
  IRLabel :: H.Label -> MateIR t C O
  IROp :: (Show t) => OpType -> t -> t -> t -> MateIR t O O
  IRNop :: MateIR t O O
  IRInvoke :: Word16 -> MateIR t O O
  IRJump :: H.Label -> MateIR t O C
  IRIfElse :: (Show t) => CMP -> t -> t -> H.Label -> H.Label -> MateIR t O C
  IRReturn :: Bool -> MateIR t O C

data LinearIns t
  = Fst (MateIR t C O)
  | Mid (MateIR t O O)
  | Lst (MateIR t O C)

data OpType
  = Add
  | Sub
  | Mul
  deriving Show

data HVar
  = HIReg Reg32
  | HIConstant Int32
  | SpillIReg Disp
  | HFReg XMMReg
  | HFConstant Float
  | SpillFReg Disp
  deriving Eq

deriving instance Eq Disp

data VarType = JInt | JFloat deriving (Show, Eq)

data Var
  = JIntValue Int32
  | JFloatValue Float
  | VReg VarType Integer

varType :: Var -> VarType
varType (JIntValue _) = JInt
varType (JFloatValue _) = JFloat
varType (VReg t _) = t

instance Functor SimpleUniqueMonad where
  fmap = liftM

instance NonLocal (MateIR Var) where
  entryLabel (IRLabel l) = l
  successors (IRJump l) = [l]
  successors (IRIfElse _ _ _ l1 l2) = [l1, l2]
  successors (IRReturn _) = []

{- show -}
instance Show (MateIR t e x) where
  show (IRLabel l) = printf "label: %s:\n" (show l)
  show (IROp op vr v1 v2) = printf "\t%s %s,  %s, %s" (show op) (show vr) (show v1) (show v2)
  show (IRInvoke x) = printf "\tinvoke %s" (show x)
  show (IRJump l) = printf "\tjump %s" (show l)
  show (IRIfElse jcmp v1 v2 l1 l2) = printf "\tif (%s `%s` %s) then %s else %s" (show v1) (show jcmp) (show v2) (show l1) (show l2)
  show (IRReturn b) = printf "\treturn (%s)" (show b)
  show IRNop = printf "\tnop"

instance Show (LinearIns t) where
  show (Fst n) = printf "%s\n" $ show n
  show (Mid n) = printf "%s\n" $ show n
  show (Lst n) = printf "%s\n" $ show n

instance Show HVar where
  show (HIReg r32) = printf "%s" (show r32)
  show (HIConstant val) = printf "0x%08x" val
  show (SpillIReg (Disp d)) = printf "0x%02x(ebp[i])" d
  show (HFReg xmm) = printf "%s" (show xmm)
  show (HFConstant val) = printf "%2.2ff" val
  show (SpillFReg (Disp d)) = printf "0x%02x(ebp[f])" d

instance Show Var where
  show (VReg t n) = printf "%s(%02d)" (show t) n
  show (JIntValue n) = printf "0x%08x" n
  show (JFloatValue n) = printf "%2.2ff" n
{- /show -}


{- make Hoopl graph from JVMInstruction -}
data SimStack = SimStack
  { stack :: [Var]
  , regcnt :: Integer }

data LabelLookup = LabelLookup 
  { labels :: M.Map Int16 H.Label
  , blockEntries :: S.Set Int16
  , simStack :: SimStack
  , instructions :: [J.Instruction]
  , pcOffset :: Int16 }

type LabelState a = StateT LabelLookup SimpleUniqueMonad a

-- mkFirst ::    GraphRep g              =>   n C O  -> g n C O
-- mkMiddle  :: (GraphRep g, NonLocal n) =>   n O O  -> g n O O
-- mkMiddles :: (GraphRep g, NonLocal n) =>  [n O O] -> g n O O
-- mkLast ::     GraphRep g =>                n O C  -> g n O C
-- (<*>) ::     (GraphRep g, NonLocal n) => g n e O  -> g n O x -> g n e x
-- (|*><*|) ::  (GraphRep g, NonLocal n) => g n e C  -> g n C x -> g n e x

w162i16 :: Word16 -> Int16
w162i16 w16 = fromIntegral w16

-- forward references wouldn't be a problem, but backwards are
resolveReferences :: LabelState ()
resolveReferences = do
    jvminsn <- instructions <$> get
    pc <- pcOffset <$> get
    if null jvminsn
      then do
        addPC 0 -- add entry instruction
        addPC pc -- mark return instruction
      else do
        let ins = head jvminsn
        addJumpTarget ins pc
        incrementPC ins
        popInstruction
        resolveReferences
  where
    addJumpTarget :: J.Instruction -> Int16 -> LabelState ()
    addJumpTarget ins pc = case ins of
        (IF _ rel) -> addPCs pc rel ins
        (IF_ICMP _ rel) -> addPCs pc rel ins
        (IF_ACMP _ rel) -> addPCs pc rel ins
        (IFNULL rel) -> addPCs pc rel ins
        (IFNONNULL rel) -> addPCs pc rel ins
        GOTO rel -> addPC (pc + w162i16 rel)
        JSR _ -> error "addJumpTarget: JSR?!"
        GOTO_W _ -> error "addJumpTarget: GOTO_W?!"
        JSR_W _ -> error "addJumpTarget: JSR_W?!"
        TABLESWITCH _ _ _ _ _ -> error "addJumpTarget: tableswitch"
        LOOKUPSWITCH _ _ _ _ -> error "addJumpTarget: lookupswitch"
        _ -> return ()
    addPCs :: Int16 -> Word16 -> J.Instruction -> LabelState ()
    addPCs pc rel ins = do addPC (pc + insnLength ins); addPC (pc + (w162i16 rel))
    addPC :: Int16 -> LabelState ()
    addPC bcoff = do
      modify (\s -> s { blockEntries = S.insert bcoff (blockEntries s) })

mkMethod :: Graph (MateIR Var) C C -> LabelState (Graph (MateIR Var) O C)
mkMethod g = do
  entryseq <- mkLast <$> IRJump <$> addLabel 0
  return $ entryseq |*><*| g

mkBlocks :: LabelState [Graph (MateIR Var) C C]
mkBlocks = do
  pc <- pcOffset <$> get
  entries <- blockEntries <$> get
  jvminsn <- instructions <$> get
  if null jvminsn
    then return []
    else if S.member pc entries
      then do
        g <- mkBlock
        gs <- mkBlocks
        return $ g : gs
      else error $ "mkBlocks: something wrong here. pc: " ++ show pc ++
                   "\ninsn: " ++ show jvminsn

mkBlock :: LabelState (Graph (MateIR Var) C C)
mkBlock = do
  pc <- pcOffset <$> get
  f' <- IRLabel <$> (addLabel pc)
  (ms', l') <- toMid
  let noNop IRNop = False; noNop _ = True
  let ms'' = filter noNop ms'
  return $ mkFirst f' <*> mkMiddles ms'' <*> mkLast l'

addLabel :: Int16 -> LabelState H.Label
addLabel boff = do
  lmap <- labels <$> get
  if M.member boff lmap
    then return $ lmap M.! boff
    else do
      label <- lift $ freshLabel
      modify (\s -> s {labels = M.insert boff label (labels s) })
      return label

popInstruction :: LabelState ()
popInstruction = do
  i <- instructions <$> get
  modify (\s -> s { instructions = tail i })

toMid :: LabelState ([MateIR Var O O], MateIR Var O C)
toMid = do
    pc <- pcOffset <$> get
    ins <- head <$> instructions <$> get
    entries <- blockEntries <$> get
    if S.member (pc + insnLength ins) entries
      then do
        (insOO, lastins) <- toLast ins
        let insOOs = case insOO of Nothing -> []; Just x -> [x]
        return (insOOs, lastins)
      else do
        insIR <- normalIns ins
        (insn, lastins) <- toMid
        return (insIR:insn, lastins)
  where
    normalIns ins = do
      st <- get
      -- st <- (trace $ printf "tir': %s\n" (show x)) get
      let (insIR, state') = runState (tir ins) (simStack st)
      put $ st { simStack = state'}
      incrementPC ins
      popInstruction
      return insIR

    toLast :: J.Instruction -> LabelState (Maybe (MateIR Var O O), MateIR Var O C)
    toLast ins = do
      pc <- pcOffset <$> get
      let ifstuff jcmp rel op1 op2 = do
            truejmp <- addLabel (pc + w162i16 rel)
            falsejmp <- addLabel (pc + insnLength ins)
            incrementPC ins
            popInstruction
            return $ (Nothing, IRIfElse jcmp op1 op2 truejmp falsejmp)
      case ins of
        RETURN -> do
          incrementPC ins
          popInstruction
          return $ (Nothing, IRReturn False)
        ARETURN -> returnSomething
        IRETURN -> returnSomething
        LRETURN -> returnSomething
        FRETURN -> returnSomething
        DRETURN -> returnSomething
        (IF _ _) -> error "toLast: IF _ _)"
        (IFNULL _) -> error "toLast: IFNULL"
        (IFNONNULL _) -> error "toLast: IFNONNULL"
        (IF_ICMP jcmp rel) -> do
          op1 <- apop2
          op2 <- apop2
          unless (varType op1 == varType op2) $ error "toLast IF_ICMP: type mismatch"
          ifstuff jcmp rel op1 op2
        (IF_ACMP jcmp rel) -> do
          op1 <- apop2
          op2 <- apop2
          unless (varType op1 == varType op2) $ error "toLast IF_ICMP: type mismatch"
          ifstuff jcmp rel op1 op2
        (GOTO _) -> do error "toLast: goto"
        _ -> do -- fallthrough case
          next <- addLabel (pc + insnLength ins)
          insIR <- normalIns ins
          return $ (Just insIR, IRJump next)
      where
        returnSomething = do
          incrementPC ins
          popInstruction
          return $ (Nothing, IRReturn True) -- todo: encode type?!

insnLength :: Integral a => J.Instruction -> a
insnLength x = case x of
  (TABLESWITCH padding _ _ _ xs) ->
    fromIntegral $ 1 {- opcode -}
                 + (fromIntegral padding)
                 + (3 * 4) {- def, low, high -}
                 + 4 * length xs {- entries -}
  (LOOKUPSWITCH padding _ _ xs) ->
    fromIntegral $ 1 {- opcode -}
                 + (fromIntegral padding)
                 + (2 * 4) {- def, n -}
                 + 8 * length xs {- pairs -}
  _ -> len
  where
    len = fromIntegral . B.length . encodeInstructions . (:[]) $ x

incrementPC :: J.Instruction -> LabelState ()
incrementPC ins = modify (\s -> s { pcOffset = pcOffset s + insnLength ins})

resetPC :: [J.Instruction] -> LabelState ()
resetPC jvmins = do
  modify (\s -> s { pcOffset = 0, instructions = jvmins })

-- helper
apop2 :: LabelState Var
apop2 = do
  st <- get
  let lol = simStack st
  modify (\s -> s { simStack = lol { stack = tail (stack lol)} } )
  return . head . stack $ lol

tir :: J.Instruction -> State SimStack (MateIR Var O O)
tir ICONST_0 = tir (BIPUSH 0)
tir ICONST_1 = tir (BIPUSH 1)
tir ICONST_2 = tir (BIPUSH 2)
tir (BIPUSH x) = do apush $ JIntValue (fromIntegral x); return IRNop
tir FCONST_0 =  do apush $ JFloatValue 0; return IRNop
tir FCONST_1 =  do apush $ JFloatValue 1; return IRNop
tir (ILOAD_ x) = tir (ILOAD (case x of I0 -> 0; I1 -> 1; I2 -> 2; I3 -> 3))
tir (ILOAD x) = do apush $ VReg JInt (fromIntegral x); return IRNop
tir (ISTORE y) = tirStore y JInt
tir (FSTORE y) = tirStore y JFloat
tir IADD = tirOpInt Add JInt
tir ISUB = tirOpInt Sub JInt
tir IMUL = tirOpInt Mul JInt
tir FADD = tirOpInt Add JFloat
tir (INVOKESTATIC ident) = return $ IRInvoke ident -- TODO...
tir x = error $ "tir: " ++ show x

tirStore w8 t = do
  x <- apop
  let nul = case t of
              JInt -> JIntValue 0
              JFloat -> JFloatValue 0
  unless (t == varType x) $ error "tirStore: type mismatch"
  return $ IROp Add (VReg t $ fromIntegral w8) x nul
tirOpInt op t = do
  x <- apop; y <- apop
  nv <- newvar t; apush nv
  unless (t == varType x && t == varType y) $ error "tirOpInt: type mismatch"
  return $ IROp op nv x y

newvar t = do
  sims <- get
  put $ sims { regcnt = regcnt sims + 1 }
  return $ VReg t $ regcnt sims
apush x = do
  sims <- get
  put $ sims { stack = x : stack sims }
apop :: State SimStack Var
apop = do
  sims <- get
  put $ sims { stack = tail $ stack sims }
  return . head . stack $ sims
{- /make hoopl graph -}

{- flatten hoople graph -}
mkLinear :: Graph (MateIR Var) O x -> [LinearIns Var] -- [Block (MateIR Var) C C]
mkLinear = concatMap lineariseBlock . postorder_dfs
  where
    -- see compiler/Lambdachine/Grin/RegAlloc.hs
    -- lineariseBlock :: Block (MateIR Var) C C -> [LinearIns Var]
    lineariseBlock block = entry_ins ++ map Mid middles ++ tail_ins
      where
        (entry, middles, tailb) = blockToNodeList block
        entry_ins :: [LinearIns Var]
        entry_ins = case entry of JustC n -> [Fst n]; NothingC -> []
        tail_ins :: [LinearIns Var]
        tail_ins = case tailb of JustC n -> [Lst n]; NothingC -> []
{- /linear -}

{- regalloc PoC -}
data MappedRegs = MappedRegs
  { regMap :: M.Map Integer HVar
  , stackCnt :: Word32 }

emptyRegs = MappedRegs M.empty 0

allIntRegs = map HIReg [eax, ecx, edx, ebx, esi, edi] :: [HVar]
allFloatRegs = map HFReg [xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7] :: [HVar]

stupidRegAlloc :: [LinearIns Var] -> [LinearIns HVar]
stupidRegAlloc linsn = evalState regAlloc' emptyRegs
  where
    regAlloc' = mapM assignReg linsn
    assignReg :: LinearIns Var -> State MappedRegs (LinearIns HVar)
    assignReg lv = case lv of
      Fst x -> case x of
        IRLabel x' -> return $ Fst $ IRLabel x'
      Mid ins -> case ins of
        IROp op dst src1 src2 -> do
          dstnew <- doAssign dst
          src1new <- doAssign src1
          src2new <- doAssign src2
          return $ Mid $ IROp op dstnew src1new src2new
        IRNop -> return $ Mid $ IRNop
        IRInvoke b -> return $ Mid $ IRInvoke b
      Lst ins -> case ins of
        IRJump l -> return $ Lst $ IRJump l
        IRIfElse jcmp cmp1 cmp2 l1 l2 -> do
          cmp1new <- doAssign cmp1
          cmp2new <- doAssign cmp2
          return $ Lst $ IRIfElse jcmp cmp1new cmp2new l1 l2
        IRReturn b -> return $ Lst $ IRReturn b

    doAssign :: Var -> State MappedRegs HVar
    doAssign (JIntValue x) = return $ HIConstant x
    doAssign (JFloatValue x) = return $ HFConstant x
    doAssign vr = do
      isAssignVr <- hasAssign vr
      if isAssignVr
        then getAssign vr
        else nextAvailReg vr
      where
        hasAssign :: Var -> State MappedRegs Bool
        hasAssign (VReg _ vreg) = M.member vreg <$> regMap <$> get
        hasAssign x = error $ "hasAssign: " ++ show x

        getAssign :: Var -> State MappedRegs HVar
        getAssign (VReg _ vreg) = (M.! vreg) <$> regMap <$> get
        getAssign x = error $ "getAssign: " ++ show x

        nextAvailReg:: Var -> State MappedRegs HVar
        nextAvailReg (VReg t vreg) = do
          availregs <- availRegs t
          mr <- get
          case availregs of
            [] -> do
              let disp = stackCnt mr
              let spill = case t of
                            JInt -> SpillIReg (Disp disp)
                            JFloat -> SpillFReg (Disp disp)
              let imap = M.insert vreg spill $ regMap mr
              put (mr { stackCnt = disp + 4, regMap = imap} )
              return spill
            (x:_) -> do
              let imap = M.insert vreg x $ regMap mr
              put (mr { regMap = imap })
              return x
        nextAvailReg _ = error "intNextReg: dafuq"

        regsInUse :: VarType -> State MappedRegs [HVar]
        regsInUse t = do
          mr <- M.elems <$> regMap <$> get
          let unpackIntReg :: HVar -> Maybe HVar
              unpackIntReg x@(HIReg _) = Just x
              unpackIntReg _ = Nothing
          let unpackFloatReg :: HVar -> Maybe HVar
              unpackFloatReg x@(HFReg _) = Just x
              unpackFloatReg _ = Nothing
          let unpacker = case t of JInt -> unpackIntReg; JFloat -> unpackFloatReg
          return . mapMaybe unpacker $ mr

        availRegs :: VarType -> State MappedRegs [HVar]
        availRegs t = do
          inuse <- regsInUse t
          let allregs = case t of JInt -> allIntRegs; JFloat -> allFloatRegs
          return (allregs L.\\ inuse)
{- /regalloc -}

{- codegen -}
compileLinear :: M.Map Int16 H.Label -> [LinearIns HVar] -> CodeGen e s [Instruction]
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
            (HIConstant c, HIReg s1) -> cmp s1 (fromIntegral c :: Word32)
            (HIReg s1, HIConstant c) -> cmp s1 (fromIntegral c :: Word32)
            x -> error $ "IRifelse: not impl. yet" ++ show x
          case jcmp of
            C_EQ -> je  l1; C_NE -> jne l1
            C_LT -> jl  l1; C_GT -> jg  l1
            C_GE -> jge l1; C_LE -> jle l1
          jmp l2
        IRJump h -> jmp (lmap M.! h)
        IRReturn _ -> ret
  mapM_ compileIns linsn
  disassemble

i322w32 :: Int32 -> Word32
i322w32 = fromIntegral

girEmitOO :: MateIR HVar O O -> CodeGen e s ()
girEmitOO (IROp Add dst' src1' src2') =
    ge dst' src1' src2'
  where
    ge :: HVar -> HVar -> HVar -> CodeGen e s ()
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
    ge (HFReg dst) (HFConstant _) (HFConstant _) = do
      newNamedLabel "TODO!" >>= defineLabel
      movss dst dst
    ge (HFReg dst) (HFReg src) (HFConstant 0) =
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
    ge _ _ _ = error $ "sub: not impl.: " ++ show dst' ++ ", "
                     ++ show src1' ++ ", " ++ show src2'
girEmitOO (IROp Mul _ _ _) = do
  newNamedLabel "TODO! IROp Mul" >>= defineLabel
  nop
girEmitOO (IRInvoke _) = do
  newNamedLabel "TODO! call" >>= defineLabel
  call (0x0 :: Word32)
girEmitOO x = error $ "girEmitOO: insn not implemented: " ++ show x
{- /codeGen -}

{- sandbox to play -}
pipeline :: [J.Instruction] -> Bool -> IO ()
pipeline jvminsn debug = do
    prettyHeader "JVM Input"
    mapM_ (printf "\t%s\n" . show) jvminsn
    when debug $ prettyHeader "Hoopl Graph"
    when debug $ printf "%s\n" (showGraph show graph)
    when debug $ prettyHeader "Label Map"
    when debug $ printf "%s\n" (show lbls)
    when debug $ prettyHeader "Flatten Graph"
    when debug $ printf "%s\n" (show linear)
    when debug $ prettyHeader "Register Allocation"
    when debug $ printf "%s\n" (show ra)
    prettyHeader "Code Generation"
    (_, Right dis) <- runCodeGen (compileLinear lbls ra) () ()
    mapM_ (printf "%s\n" . showIntel) dis
  where
    initstate = LabelLookup { labels = M.empty
                            , blockEntries = S.empty
                            , simStack = SimStack [] 50000
                            , instructions = jvminsn
                            , pcOffset = 0 }
    -- transform = foldl (liftM2 (|*><*|)) (return emptyClosedGraph) mkBlocks
    runAll prog = (runSimpleUniqueMonad . runStateT prog) initstate
    (graph, transstate) = runAll $ do
      resolveReferences
      refs <- blockEntries <$> get
      trace (printf "refs: %s\n" (show refs)) $ resetPC jvminsn
      -- modify (\s -> s { blockEntries = S.fromList [0, 5, 25, 34, 61] })
      gs <- mkBlocks
      let g = foldl (|*><*|) emptyClosedGraph gs
      mkMethod g
    lbls = labels transstate
    linear = mkLinear graph
    ra = stupidRegAlloc linear

prettyHeader :: String -> IO ()
prettyHeader str = do
  let len = length str + 6
  replicateM_ len (putChar '-'); putStrLn ""
  printf "-- %s --\n" str
  replicateM_ len (putChar '-'); putStrLn ""
  -- putStrLn "press any key to continue..." >> getChar
  return ()

main :: IO ()
main = do
  cls <- parseClassFile "../tests/Fib.class"
  case lookupMethod "fib" cls of
    Just m -> do
      let code = codeInstructions $ decodeMethod $ fromMaybe (error "no code seg") (attrByName m "Code")
      pipeline code True
    Nothing -> error "lookupMethod"
{- /application -}
