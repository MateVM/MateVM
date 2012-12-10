{-# LANGUAGE OverloadedStrings #-}

module Compiler.Mate.Debug
  ( printfJit
  , printfTrap
  , printfBb
  , printfMp
  , printfCp
  , printfStr
  , printfInfo
  , printfEx
  , printfGc
  , printfMem
  , printfPipe
  , tracePipe
  , mateDEBUG
  , logGcT
  , gcLogEnabled 
  , printfPlain
  , showRefs
  , printf -- TODO: delete me
  ) where

import Text.Printf
import System.IO
import System.IO.Unsafe
import Control.Monad
import Control.Monad.State
import Foreign hiding(unsafePerformIO)

{-# NOINLINE logHandle #-}
-- TODO(bernhard): use MVar if threaded
logHandle :: Handle
logHandle = if mateDEBUG
    then unsafePerformIO $ openFile "mate.log" WriteMode
    else stdout

{-# INLINE mateDEBUG #-}
mateDEBUG :: Bool
mateDEBUG = False

{-# INLINE printString #-}
printString :: String -> String -> IO ()
printString prefix str = do
  when mateDEBUG $ hPutStr logHandle . (++) prefix $ str
  hFlush logHandle

{-# INLINE printfJit #-}
{-# INLINE printfTrap #-}
{-# INLINE printfBb #-}
{-# INLINE printfMp #-}
{-# INLINE printfCp #-}
{-# INLINE printfStr #-}
{-# INLINE printfInfo #-}
{-# INLINE printfEx #-}
{-# INLINE printfGc #-}
{-# INLINE printfMem #-}
{-# INLINE printfPipe #-}
{-# INLINE printfPlain #-}
printfJit, printfTrap, printfBb, printfMp, printfCp,
  printfStr, printfInfo, printfEx, printfPipe, printfMem, printfGc, printfPlain :: String -> IO ()
{-
-- TODO(bernhard):
-- http://stackoverflow.com/questions/12123082/function-composition-with-text-printf-printf
-}
printfJit  = printString "Jit: "
printfTrap = printString "Trap: "
printfBb   = printString "Bb: "
printfMp   = printString "Mp: "
printfCp   = printString "Cp: "
printfStr  = printString "Str: "
printfInfo = printString "Info: "
printfEx = printString "Ex: "
printfGc = printString "Gc: "
printfMem = printString "Mem: "
printfPipe = printString "Pipe: "
printfPlain = printString ""

{-# NOINLINE tracePipe #-}
tracePipe :: String -> a -> a
tracePipe string expr = unsafePerformIO $ do
  printfPipe string
  return expr
{-# INLINE gcLogEnabled #-}
gcLogEnabled :: Bool
gcLogEnabled = True

logGcT :: String -> StateT b IO ()
logGcT s = when gcLogEnabled (liftIO $ printfGc s)

showRefs :: [IntPtr] -> [String]
showRefs = map (show . intPtrToPtr) 
