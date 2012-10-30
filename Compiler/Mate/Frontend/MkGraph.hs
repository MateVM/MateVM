{- make Hoopl graph from J.Instruction.
   this includes:
     * type analysis of stack values
     * introducing (typed) virtual registers and constants
-}
module Compiler.Mate.Frontend.MkGraph
  ( SimStack(..)
  , LabelLookup(..)
  , resolveReferences
  , resetPC
  , mkBlocks
  , mkMethod
  ) where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Data.Int
import Data.Word

import Control.Applicative hiding ((<*>))
import Control.Monad
import Control.Monad.State

import qualified JVM.Assembler as J
import JVM.Assembler hiding (Instruction)
import JVM.ClassFile
import Compiler.Hoopl
import Harpy hiding (Label)

import Compiler.Mate.Frontend.IR
import Compiler.Mate.Frontend.StupidRegisterAllocation

import Debug.Trace
import Text.Printf

data SimStack = SimStack
  { stack :: [Var]
  , regcnt :: Integer
  , classf :: Class Direct
  , method :: Method Direct
  , preRegs :: [(Integer, HVar)]
  }

data LabelLookup = LabelLookup
  { labels :: M.Map Int32 Label
  , blockEntries :: S.Set Int32
  , simStack :: SimStack
  , instructions :: [J.Instruction]
  , pcOffset :: Int32 }

type LabelState a = StateT LabelLookup SimpleUniqueMonad a

-- mkFirst ::    GraphRep g              =>   n C O  -> g n C O
-- mkMiddle  :: (GraphRep g, NonLocal n) =>   n O O  -> g n O O
-- mkMiddles :: (GraphRep g, NonLocal n) =>  [n O O] -> g n O O
-- mkLast ::     GraphRep g =>                n O C  -> g n O C
-- (<*>) ::     (GraphRep g, NonLocal n) => g n e O  -> g n O x -> g n e x
-- (|*><*|) ::  (GraphRep g, NonLocal n) => g n e C  -> g n C x -> g n e x

w162i32 :: Word16 -> Int32
w162i32 w16 = fromIntegral i16
  where i16 = fromIntegral w16 :: Int16

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
        when (null jvminsn) $ error "resolveReferences: something is really wrong here"
        let ins = head jvminsn
        addJumpTarget ins pc
        incrementPC ins
        popInstruction
        resolveReferences
  where
    addJumpTarget :: J.Instruction -> Int32 -> LabelState ()
    addJumpTarget ins pc = case ins of
        (IF _ rel) -> addPCs pc rel ins
        (IF_ICMP _ rel) -> addPCs pc rel ins
        (IF_ACMP _ rel) -> addPCs pc rel ins
        (IFNULL rel) -> addPCs pc rel ins
        (IFNONNULL rel) -> addPCs pc rel ins
        GOTO rel -> addPC (pc + w162i32 rel)
        JSR _ -> error "addJumpTarget: JSR?!"
        GOTO_W _ -> error "addJumpTarget: GOTO_W?!"
        JSR_W _ -> error "addJumpTarget: JSR_W?!"
        TABLESWITCH _ _ _ _ _ -> error "addJumpTarget: tableswitch"
        LOOKUPSWITCH _ _ _ _ -> error "addJumpTarget: lookupswitch"
        _ -> return ()
    addPCs :: Int32 -> Word16 -> J.Instruction -> LabelState ()
    addPCs pc rel ins = do addPC (pc + insnLength ins); addPC (pc + (w162i32 rel))
    addPC :: Int32 -> LabelState ()
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
  f' <- IRLabel <$> addLabel pc
  (ms', l') <- toMid
  return $ mkFirst f' <*> mkMiddles ms' <*> mkLast l'

addLabel :: Int32 -> LabelState Label
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
  when (null i) $ error "popInstruction: something is really wrong here"
  modify (\s -> s { instructions = tail i })

toMid :: LabelState ([MateIR Var O O], MateIR Var O C)
toMid = do
    pc <- pcOffset <$> get
    insns <- instructions <$> get
    when (null insns) $ error "toMid: something is really wrong here :/"
    ins <- head <$> instructions <$> get
    entries <- blockEntries <$> get
    if S.member (pc + insnLength ins) entries
      then toLast ins
      else do
        insIR <- normalIns ins
        (insn, lastins) <- toMid
        return (insIR ++ insn, lastins)
  where
    normalIns ins = do
      st <- get
      -- st <- (trace $ printf "tir': %s\n" (show x)) get
      let (insIR, state') = runState (tir ins) (simStack st)
      put $ st { simStack = state'}
      incrementPC ins
      popInstruction
      return insIR

    toLast :: J.Instruction -> LabelState ([MateIR Var O O], MateIR Var O C)
    toLast ins = do
      pc <- pcOffset <$> get
      let ifstuff jcmp rel op1 op2 = do
            truejmp <- addLabel (pc + w162i32 rel)
            falsejmp <- addLabel (pc + insnLength ins)
            incrementPC ins
            popInstruction
            return $ ([], IRIfElse jcmp op1 op2 truejmp falsejmp)
      case ins of
        RETURN -> do
          incrementPC ins
          popInstruction
          return $ ([], IRReturn Nothing)
        ARETURN -> returnSomething JRef
        IRETURN -> returnSomething JInt
        LRETURN -> error "toLast: LReturn"
        FRETURN -> returnSomething JFloat
        DRETURN -> error "toLast: DReturn"
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
          return $ (insIR, IRJump next)
      where
        returnSomething t = do
          incrementPC ins
          popInstruction
          r <- apop2
          unless (varType r == t) $ error "toLast return: type mismatch"
          return $ ([], IRReturn $ Just r)

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
  when (null . stack $ lol) $ error "apop2: something is really wrong here"
  modify (\s -> s { simStack = lol { stack = tail (stack lol)} } )
  return . head . stack $ lol

imm2num :: Num a => IMM -> a
imm2num I0 = 0
imm2num I1 = 1
imm2num I2 = 2
imm2num I3 = 3

fieldType :: Class Direct -> Word16 -> VarType
fieldType cls off = fieldType2VarType $ ntSignature nt
  where nt = case constsPool cls M.! off of
                (CField _ nt') -> nt'
                _ -> error "fieldType: fail :("

methodType :: Bool -> Class Direct -> Word16 -> ([VarType], Maybe VarType)
methodType isVirtual cls off = (map fieldType2VarType argst', rett)
  where
    argst' = if isVirtual then (ObjectType "lol"):argst else argst
    (MethodSignature argst returnt) =
      case constsPool cls M.! off of
        (CMethod _ nt') -> ntSignature nt'
        _ -> error "methodType: fail :("
    rett = case returnt of
            Returns ft -> Just (fieldType2VarType ft)
            ReturnsVoid -> Nothing

methodIsStatic :: Method Direct -> Bool
methodIsStatic = S.member ACC_STATIC . methodAccessFlags

methodArgs :: Num a => Method Direct -> a
methodArgs meth = isStatic $ L.genericLength args
  where
    (MethodSignature args _) = methodSignature meth
    isStatic = if methodIsStatic meth then (+0) else (+1)

fieldType2VarType :: FieldType -> VarType
fieldType2VarType IntType = JInt
fieldType2VarType FloatType = JFloat
fieldType2VarType (ObjectType _) = JRef
fieldType2VarType (Array _ ty) = fieldType2VarType ty
fieldType2VarType x = error $ "fieldType2VarType: " ++ show x

tir :: J.Instruction -> State SimStack [MateIR Var O O]
tir ICONST_0 = tir (BIPUSH 0)
tir ICONST_1 = tir (BIPUSH 1)
tir ICONST_2 = tir (BIPUSH 2)
tir ICONST_3 = tir (BIPUSH 3)
tir (BIPUSH x) = do apush $ JIntValue (fromIntegral x); return []
tir (SIPUSH x) = do apush $ JIntValue (fromIntegral x); return []
tir FCONST_0 =  do apush $ JFloatValue 0; return []
tir FCONST_1 =  do apush $ JFloatValue 1; return []
tir FCONST_2 =  do apush $ JFloatValue 3; return []
tir (ILOAD_ x) = tir (ILOAD (imm2num x))
tir (ILOAD x) = tirLoad x JInt
tir (ALOAD_ x) = tir (ALOAD (imm2num x))
tir (ALOAD x) = tirLoad x JRef
tir (FLOAD_ x) = tir (FLOAD (imm2num x))
tir (FLOAD x) = tirLoad x JFloat
tir (ISTORE_ x) = tir (ISTORE (imm2num x))
tir (ISTORE y) = tirStore y JInt
tir (FSTORE_ y) = tir (FSTORE (imm2num y))
tir (FSTORE y) = tirStore y JFloat
tir (ASTORE_ x) = tir (ASTORE (imm2num x))
tir (ASTORE x) = tirStore x JRef
tir (PUTFIELD x) = do
  src <- apop
  obj <- apop
  unless (JRef == varType obj) $ error "putfield: type mismatch"
  cls <- classf <$> get
  unless (fieldType cls x == varType src) $ error "putfield: type mismatch2"
  return [IRStore (RTPool x) obj src]
tir (GETFIELD x) = do
  obj <- apop
  unless (JRef == varType obj) $ error "getfield: type mismatch"
  cls <- classf <$> get
  nv <- newvar (fieldType cls x)
  apush nv
  return [IRLoad (RTPool x) obj nv]
tir (GETSTATIC x) = do
  cls <- classf <$> get
  nv <- newvar (fieldType cls x)
  apush nv
  return [IRLoad (RTPool x) JRefNull nv]
tir (PUTSTATIC x) = do
  y <- apop
  return [IRStore (RTPool x) JRefNull y]
tir (LDC1 x) = tir (LDC2 (fromIntegral x))
tir (LDC2 x) = do
  cls <- classf <$> get
  let valuetype = case constsPool cls M.! x of
            (CString _) -> JRef
            (CInteger _) -> JInt
            e -> error $ "tir: LDCI... missing impl.: " ++ show e
  nv <- newvar valuetype
  apush nv
  return [IRLoad (RTPool x) JRefNull nv]
tir (NEW x) = do
  nv <- newvar JRef
  apush nv
  return [IRLoad (RTPool x) JRefNull nv]
tir (ANEWARRAY w16) = tir (NEWARRAY 10) -- for int. TODO?
tir (NEWARRAY w8) = do
  len <- apop
  let len' = case len of
              JIntValue x -> fromIntegral x
              x -> error $ "tir: anewarray: len is not constant: " ++ show x
  nv <- newvar JRef
  apush nv
  return [IRLoad (RTArray w8 len') JRefNull nv]
tir DUP = do
  x <- apop
  apush x
  nv <- newvar (varType x)
  apush nv
  return [IROp Add nv x (JIntValue 0)]
tir POP = do apop; return []
tir IADD = tirOpInt Add JInt
tir ISUB = tirOpInt Sub JInt
tir IMUL = tirOpInt Mul JInt
tir FADD = tirOpInt Add JFloat
tir (INVOKESTATIC ident) = tirInvoke False ident
tir (INVOKESPECIAL ident) = tirInvoke False ident
tir (INVOKEVIRTUAL ident) = tirInvoke True ident
tir x = error $ "tir: " ++ show x

tirInvoke :: Bool -> Word16 -> State SimStack [MateIR Var O O]
tirInvoke isVirtual ident = do
  cls <- classf <$> get
  let (varts, mret) = methodType isVirtual cls ident
  pushes <- trace (printf "tirInvoke: varts: %s\n" (show varts)) $ 
            forM (reverse $ zip varts [0..]) $ \(x, nr) -> do
    y <- apop
    unless (x == varType y) $ error "invoke: type mismatch"
    case x of
      JInt -> return $ IRPush nr y
      JRef -> return $ IRPush nr y
      JFloat -> do
        let nr8 = fromIntegral nr
        let nri = fromIntegral nr
        let assign = preFloats !! nri
        modify (\s -> s { preRegs = (assign, HFReg $ XMMReg nr8) : (preRegs s) })
        return $ IROp Add (VReg x assign) y (JFloatValue 0) -- mov
  -- TODO: reverse pushes again, for x86 call conv stuff?
  (targetreg, maybemov) <- case mret of
    Just x -> do
      let prereg = case x of
                      JInt -> preeax
                      JFloat -> prexmm7
                      JRef -> preeax
      let nv = VReg x prereg
      movtarget <- newvar x
      apush movtarget
      let movretval = IROp Add movtarget nv (JIntValue 0)
      return (Just nv, Just movretval)
    Nothing -> return (Nothing, Nothing)
  let r = pushes ++ [IRInvoke (RTPool ident) targetreg]
  case maybemov of
    Nothing -> return r
    Just m -> return $ r ++ [m]

tirLoad :: Word8 -> VarType -> State SimStack [MateIR Var O O]
tirLoad x t = do
  meth <- method <$> get
  vreg <- if x < methodArgs meth
           then do
             case t of
              JFloat -> do
                let assign = preFloats !! (fromIntegral x)
                let tup = (assign, HFReg . XMMReg . fromIntegral $ x)
                modify (\s -> s { preRegs = tup : (preRegs s) })
                return $ VReg t assign
              _ -> do
                let assign = preArgs !! (fromIntegral x)
                let tup = (assign, SpillIReg . Disp . fromIntegral $ (ptrSize * x))
                modify (\s -> s { preRegs = tup : (preRegs s) })
                return $ VReg t assign
           else return $ VReg t (fromIntegral x)
  apush vreg
  return []

tirStore :: Word8 -> VarType -> State SimStack [MateIR Var O O]
tirStore w8 t = do
  x <- apop
  let nul = case t of
              JInt -> JIntValue 0
              JFloat -> JFloatValue 0
              JRef -> JRefNull
  unless (t == varType x) $ error "tirStore: type mismatch"
  return [IROp Add (VReg t $ fromIntegral w8) x nul]

tirOpInt :: OpType -> VarType -> State SimStack [MateIR Var O O]
tirOpInt op t = do
  x <- apop; y <- apop
  nv <- newvar t; apush nv
  unless (t == varType x && t == varType y) $ error "tirOpInt: type mismatch"
  return [IROp op nv x y]

newvar :: VarType -> State SimStack Var
newvar t = do
  sims <- get
  put $ sims { regcnt = regcnt sims + 1 }
  return $ VReg t $ regcnt sims

apush :: Var -> State SimStack ()
apush x = do
  s <- stack <$> get
  sims <- get
  put $ sims { stack = x : s }

apop :: State SimStack Var
apop = do
  (s:ss) <- stack <$> get
  modify (\m -> m { stack = ss })
  return s
