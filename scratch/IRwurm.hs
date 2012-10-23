{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Int
import Data.Word
import Data.Maybe
import Control.Applicative hiding ((<*>))

import Harpy
import Harpy.X86Disassembler

import Compiler.Hoopl hiding (Label)
import qualified Compiler.Hoopl as H

import Control.Monad.State

import Debug.Trace
import Text.Printf

{- TODO
(.) typeclass for codeemitting: http://pastebin.com/RZ9qR3k7 (depricated) || http://pastebin.com/BC3Jr5hG
(.) backref resolution before JavaStackSim
-}

-- source IR (jvm bytecode)
data JVMInstruction
  = ICONST_0 | ICONST_1
  | FCONST_0 | FCONST_1
  | IPUSH Int32
  | ILOAD Word8  -- storage offset
  | FLOAD Word8  -- storage offset
  | ISTORE Word8 -- storage offset
  | FSTORE Word8 -- storage offset
  | IADD | ISUB | IMUL | FADD
  | IFEQ_ICMP Int16 -- signed relative offset
  | GOTO Int16
  | DUP | SWAP
  | INVOKE Word8 -- amount of arguments
  | RETURN
  deriving Show

data MateIR t e x where
  IRLabel :: H.Label -> MateIR t C O
  IROp :: (Show t) => OpType -> t -> t -> t -> MateIR t O O
  IRNop :: MateIR t O O
  IRInvoke :: Word8 -> MateIR t O O
  IRJump :: H.Label -> MateIR t O C
  IRIfElse :: (Show t) => t -> t -> H.Label -> H.Label -> MateIR t O C
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
  successors (IRIfElse _ _ l1 l2) = [l1, l2]
  successors (IRReturn _) = []

{- show -}
instance Show (MateIR t e x) where
  show (IRLabel l) = printf "label: %s:\n" (show l)
  show (IROp op vr v1 v2) = printf "\t%s %s,  %s, %s" (show op) (show vr) (show v1) (show v2)
  show (IRInvoke x) = printf "\tinvoke %s" (show x)
  show (IRJump l) = printf "\tjump %s" (show l)
  show (IRIfElse v1 v2 l1 l2) = printf "\tif (%s == %s) then %s else %s" (show v1) (show v2) (show l1) (show l2)
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
  , instructions :: [JVMInstruction]
  , pcOffset :: Int16 }

type LabelState a = StateT LabelLookup SimpleUniqueMonad a

-- mkFirst ::    GraphRep g              =>   n C O  -> g n C O
-- mkMiddle  :: (GraphRep g, NonLocal n) =>   n O O  -> g n O O
-- mkMiddles :: (GraphRep g, NonLocal n) =>  [n O O] -> g n O O
-- mkLast ::     GraphRep g =>                n O C  -> g n O C
-- (<*>) ::     (GraphRep g, NonLocal n) => g n e O  -> g n O x -> g n e x
-- (|*><*|) ::  (GraphRep g, NonLocal n) => g n e C  -> g n C x -> g n e x

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
    addJumpTarget :: JVMInstruction -> Int16 -> LabelState ()
    addJumpTarget ins pc = case ins of
        IFEQ_ICMP rel -> do
          addPC (pc + insnLength ins)
          addPC (pc + rel)
        GOTO rel -> addPC (pc + rel)
        _ -> return ()
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

    toLast :: JVMInstruction -> LabelState (Maybe (MateIR Var O O), MateIR Var O C)
    toLast ins = do
      pc <- pcOffset <$> get
      case ins of
        RETURN -> do
          incrementPC ins
          popInstruction
          return $ (Nothing, IRReturn True)
        (IFEQ_ICMP rel) -> do
          x <- apop2
          y <- apop2
          unless (varType x == varType y) $ error "toLast IFEQ_ICMP: type mismatch"
          truejmp <- addLabel (pc + rel)
          falsejmp <- addLabel (pc + insnLength ins)
          incrementPC ins
          popInstruction
          return $ (Nothing, IRIfElse x y truejmp falsejmp)
        (GOTO rel) -> do error "toLast: goto"
        _ -> do -- fallthrough case
          next <- addLabel (pc + insnLength ins)
          insIR <- normalIns ins
          return $ (Just insIR, IRJump next)

-- dummy
insnLength :: JVMInstruction -> Int16
insnLength ICONST_0 = 2
insnLength ICONST_1 = 2
insnLength _ = 3

incrementPC :: JVMInstruction -> LabelState ()
incrementPC ins = modify (\s -> s { pcOffset = pcOffset s + insnLength ins})

resetPC :: [JVMInstruction] -> LabelState ()
resetPC jvmins = do
  modify (\s -> s { pcOffset = 0, instructions = jvmins })

-- helper
apop2 :: LabelState Var
apop2 = do
  st <- get
  let lol = simStack st
  modify (\s -> s { simStack = lol { stack = tail (stack lol)} } )
  return . head . stack $ lol

tir :: JVMInstruction -> State SimStack (MateIR Var O O)
tir ICONST_0 = tir (IPUSH 0)
tir ICONST_1 = tir (IPUSH 1)
tir (IPUSH x) = do apush $ JIntValue x; return IRNop
tir FCONST_0 =  do apush $ JFloatValue 0; return IRNop
tir FCONST_1 =  do apush $ JFloatValue 1; return IRNop
tir (ILOAD x) = do apush $ VReg JInt (fromIntegral x); return IRNop
tir (ISTORE y) = tirStore y JInt
tir (FSTORE y) = tirStore y JFloat
tir IADD = tirOpInt Add JInt
tir ISUB = tirOpInt Sub JInt
tir IMUL = tirOpInt Mul JInt
tir FADD = tirOpInt Add JFloat
tir (IFEQ_ICMP _) = error "if in middle of block"
tir RETURN = error "return in middle of block" -- return $ IRReturn False
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
        IRIfElse cmp1 cmp2 l1 l2 -> do
          cmp1new <- doAssign cmp1
          cmp2new <- doAssign cmp2
          return $ Lst $ IRIfElse cmp1new cmp2new l1 l2
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
        IRIfElse src1 src2 h1 h2 -> do
          let l1 = lmap M.! h1
          let l2 = lmap M.! h2
          case (src1, src2) of
            (HIReg s1, HIReg s2) -> cmp s1 s2
            (HIConstant c, HIReg s1) -> cmp s1 (fromIntegral c :: Word32)
            (HIReg s1, HIConstant c) -> cmp s1 (fromIntegral c :: Word32)
            x -> error $ "IRifelse: not impl. yet" ++ show x
          je l1
          jmp l2
        IRJump h -> jmp (lmap M.! h)
        IRReturn _ -> ret
  mapM_ compileIns linsn
  disassemble

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
girEmitOO (IROp Mul _ _ _) = do
  newNamedLabel "TODO!" >>= defineLabel
  nop
girEmitOO x = error $ "girEmitOO: insn not implemented: " ++ show x
{- /codeGen -}

{- sandbox to play -}
jvm0 = [ICONST_0, ICONST_1, IADD, RETURN]

jvm1 = jvm1_1 ++ jvm1_2
jvm1_1 = [ICONST_0, ICONST_1, IADD, ICONST_1, IFEQ_ICMP (-9)]
jvm1_2 = [ICONST_1, RETURN]

jvm2 = jvm2_1 ++ jvm2_2 ++ jvm2_3
jvm2_1 = [ICONST_0, ISTORE 0 -- 5
         , ILOAD 0, ICONST_1, IADD, ISTORE 0 -- 16
         , IPUSH 10, ILOAD 0, IFEQ_ICMP (-17)] -- 25
jvm2_2 = [ILOAD 0, IPUSH 20, IFEQ_ICMP (-6)] -- 34
jvm2_3 = [FCONST_0, FCONST_1, FADD, FSTORE 1 -- 46
         , IPUSH 20, IPUSH 1, IMUL, ISTORE 2 -- 58
         , RETURN] -- 61

jvm3 = [ICONST_0, ISTORE 0]
    ++ [ILOAD 0, ICONST_1, IADD, ISTORE 0
       ,ILOAD 0, IPUSH 10, IFEQ_ICMP (-17)] -- len: 20
    ++ [ILOAD 0, IPUSH 20, IFEQ_ICMP (-26)]
    ++ [FCONST_0, FCONST_1, FADD, FSTORE 1, IPUSH 20
       ,IPUSH 1, IMUL, ISTORE 2]
    ++ regpressure
    ++ [RETURN]
  where
    regpressure = concat $ replicate 5 [ILOAD 0, ILOAD 0, IADD, ISTORE 0]

pipeline :: [JVMInstruction] -> Bool -> IO ()
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
  -- printf "\n\n\njvm1 example\n"
  -- pipeline jvm1 True
  printf "\n\n\njvm2 example\n"
  pipeline jvm2 True
  printf "\n\n\njvm3 example\n"
  pipeline jvm3 True
{- /application -}
