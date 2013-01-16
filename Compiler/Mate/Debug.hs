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
  , printfPlain
  , printfTime
  , tracePipe
  , mateDEBUG
  , mateTIME
  , logGcT
  , gcLogEnabled 
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

{-# INLINE mateTIME #-}
mateTIME :: Bool
mateTIME = False

{-# INLINE printError #-}
printError :: Bool -> String -> String -> IO ()
printError dbgflag prefix str = do
  when dbgflag $ hPutStr stderr . (++) prefix $ str
  hFlush stderr

{-# INLINE printString #-}
printString :: Bool -> String -> String -> IO ()
printString dbgflag prefix str = do
  when dbgflag $ hPutStr logHandle . (++) prefix $ str
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
  printfStr, printfInfo, printfEx, printfPipe, printfMem, printfGc,
  printfPlain, printfTime :: String -> IO ()
{-
-- TODO(bernhard):
-- http://stackoverflow.com/questions/12123082/function-composition-with-text-printf-printf
-}
printfJit  = printString mateDEBUG "Jit: "
printfTrap = printString mateDEBUG "Trap: "
printfBb   = printString mateDEBUG "Bb: "
printfMp   = printString mateDEBUG "Mp: "
printfCp   = printString mateDEBUG "Cp: "
printfStr  = printString mateDEBUG "Str: "
printfInfo = printString mateDEBUG "Info: "
printfEx   = printString mateDEBUG "Ex: "
printfGc   = printString mateDEBUG "Gc: "
printfMem  = printString mateDEBUG "Mem: "
printfPipe = printString mateDEBUG "Pipe: "
printfPlain = printString mateDEBUG ""
printfTime = printError mateTIME "time: "

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
