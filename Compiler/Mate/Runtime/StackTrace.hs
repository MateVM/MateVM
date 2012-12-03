module Compiler.Mate.Runtime.StackTrace
  ( StackDescription(..)
  , stackFrames
  , printStackTrace'
  , printStackTrace
  , printStackFramesPrecise
  )  where

import Foreign
import Foreign.C
import Control.Monad
import qualified Data.Map as M
import Data.String.Utils

import JVM.ClassFile -- because toString

import Compiler.Mate.Debug
import Compiler.Mate.Types

data StackDescription = StackDescription { --base :: CPtrdiff, end :: CPtrdiff, 
                                           stackinfo :: RuntimeStackInfo,
                                           candidates :: [IntPtr] } deriving Show

cPtrToIntPtr :: CPtrdiff -> Ptr a 
cPtrToIntPtr = intPtrToPtr . fromIntegral

-- accumulator (tailrecursive) stackframes stream (may be written as predefined function?)
stackFrames :: [StackDescription] -> CPtrdiff -> CPtrdiff -> IO [StackDescription]
stackFrames accum reip rebp = do 
    stblptr <- peek (cPtrToIntPtr rebp) :: IO Word32
    nextreip <- peek (cPtrToIntPtr (rebp + 0x8)) :: IO Word32
    let sptr = castPtrToStablePtr $ intPtrToPtr $ fromIntegral stblptr
    stackinfo' <- deRefStablePtr sptr :: IO RuntimeStackInfo
    printfMem $ printf "stackFrames: eip: %08x\n" (fromIntegral reip :: Word32)
    printfMem $ printf "stackFrames: elem:\n"
    candidates' <- case M.lookup (fromIntegral reip) (rsiGCPoints stackinfo') of
      Nothing -> do printfMem $ printf "stackFrames: no entry found :(\n"
                    return []
      Just points -> do
        printfMem $ printf "stackFrames: found entry \\o/\n"
        forM_ points $ \x -> do
          let point :: Word32
              -- TODO(bernhard): I'm not sure here, maybe s/rebp/prevRbp/
              point = x + fromIntegral rebp
          printfMem $ printf "stackFrames: candidate: %08x\n" point
        return $ map ((+ fromIntegral rebp) . fromIntegral) points
    let accum' = StackDescription { stackinfo = stackinfo', candidates = candidates' } : accum
    if bottomOfStack stackinfo'
     then return accum' -- done here. bottomOfStack claims that there are no frames left
     else -- otherwise grab the next frame, put current frame into list and continue
          peek (cPtrToIntPtr (rebp + 4)) >>= stackFrames accum' (fromIntegral nextreip)

-- Prints precice stacktrace to printStr. Furthermore a list
-- of stackdescriptions is produced
printStackTrace' :: CPtrdiff -> CPtrdiff -> IO [StackDescription]
printStackTrace' eip ptr = do 
  printfStr "Stacktrace:\n"
  frames <- stackFrames [] eip ptr -- build with cps toget rid of reverse?
  forM_ (reverse frames) (printfStr . printf "---> %s\n" . toString . rsiMethodname . stackinfo)  
  printfStr "End of Stack\n"        
  printStackFramesPrecise frames
  return frames

printStackFramesPrecise :: [StackDescription] -> IO ()
printStackFramesPrecise = mapM_ printPrecise
  where printPrecise f = do
          --let refs = possibleRefs f
          printfStr $ printf "Method: %s\n" (name f)
          --printfStr $ refsToString refs
        name = toString . rsiMethodname . stackinfo

{-        
refsToString :: [IntPtr] -> String
refsToString ptrs = printf "Reference Candidates: %s\n" (ptrStr ptrs)
  where ptrStr = intercalate "," . map printElement
        printElement ptr = printf "0x%08x" (fromIntegral ptr :: Word32)
-}

bottomOfStack :: RuntimeStackInfo -> Bool
bottomOfStack = mainOrInit . toString . rsiMethodname

-- | Determines wheter a method signature (as found in RuntimeStackInfos) is bottom of stack
mainOrInit :: String -> Bool
mainOrInit sig | startswith "main" sig    = True
               | startswith "<clinit>" sig = True
               | otherwise = False

-- | Prints stacktrace until bottom of stack is reached (or native code transition or trap [TODO]
-- The Int argument describes current stack depth (for pretty printing)
printStackTrace :: Int -> CPtrdiff -> IO ()
printStackTrace depth rebp = do 
    stblptr <- peek (intPtrToPtr . fromIntegral $ rebp) :: IO Word32
    let sptr = castPtrToStablePtr $ intPtrToPtr $ fromIntegral stblptr
    stackinfo' <- deRefStablePtr sptr :: IO RuntimeStackInfo
    bottomOfStack' <- printFrame depth mainOrInit stackinfo'
    unless bottomOfStack' continue
  where continue = peek (intPtrToPtr . fromIntegral $ (rebp + 4)) >>= printStackTrace (depth+1)

-- | Prints stackframe to printStr. Returns True if bottom of the stack (i.e. main)
-- is reached.
printFrame :: Int -> (String -> Bool) -> RuntimeStackInfo -> IO Bool
printFrame d bottomCheck = print' . toString . rsiMethodname
  where print' sig  | bottomCheck sig 
                      = printfStr (printf "reached bottom of stack [%d]\n" d) >> return True
                    | otherwise 
                      = printfStr (printf "stacktrace @ malloc: %s [%d]\n" sig d) >> return False
