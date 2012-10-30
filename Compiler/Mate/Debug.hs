{-# LANGUAGE OverloadedStrings #-}

module Compiler.Mate.Debug
  ( printfJit
  , printfBb
  , printfMp
  , printfCp
  , printfStr
  , printfInfo
  , printfEx
  , printfPipe
  , tracePipe
  , mateDEBUG
  , usePreciseGC
  , printf -- TODO: delete me
  ) where

import Text.Printf
import System.IO
import System.IO.Unsafe
import Control.Monad


{-# NOINLINE logHandle #-}
-- TODO(bernhard): use MVar if threaded
logHandle :: Handle
logHandle = if mateDEBUG
    then unsafePerformIO $ openFile "mate.log" WriteMode
    else stdout

{-# INLINE mateDEBUG #-}
mateDEBUG :: Bool
mateDEBUG = True

{-# INLINE printString #-}
printString :: String -> String -> IO ()
printString prefix str = do
  when mateDEBUG $ hPutStr logHandle . (++) prefix $ str
  hFlush logHandle

{-# INLINE printfJit #-}
{-# INLINE printfBb #-}
{-# INLINE printfMp #-}
{-# INLINE printfCp #-}
{-# INLINE printfStr #-}
{-# INLINE printfInfo #-}
{-# INLINE printfEx #-}
{-# INLINE printfPipe #-}
printfJit, printfBb, printfMp, printfCp,
  printfStr, printfInfo, printfEx, printfPipe :: String -> IO ()
{-
-- TODO(bernhard):
-- http://stackoverflow.com/questions/12123082/function-composition-with-text-printf-printf
-}
printfJit  = printString "Jit: "
printfBb   = printString "Bb: "
printfMp   = printString "Mp: "
printfCp   = printString "Cp: "
printfStr  = printString "Str: "
printfInfo = printString "Info: "
printfEx = printString "Ex: "
printfPipe = printString "Pipe: "

{-# NOINLINE tracePipe #-}
tracePipe :: String -> a -> a
tracePipe string expr = unsafePerformIO $ do
  printfPipe string
  return expr

{-# INLINE usePreciseGC #-}
usePreciseGC :: Bool
usePreciseGC = False
