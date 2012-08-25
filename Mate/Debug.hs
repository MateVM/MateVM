{-# LANGUAGE OverloadedStrings #-}

module Mate.Debug
  ( printfJit
  , printfBb
  , printfMp
  , printfCp
  , printfStr
  , printfInfo
  , mateDEBUG
  , printf -- TODO: delete me
  ) where

import Text.Printf
import System.IO
import System.IO.Unsafe


{-# NOINLINE logHandle #-}
-- TODO(bernhard): use MVar if threaded
logHandle :: Handle
logHandle = unsafePerformIO $ openFile "mate.log" WriteMode

{-# INLINE mateDEBUG #-}
mateDEBUG :: Bool
mateDEBUG = False

{-# INLINE printString #-}
printString :: String -> String -> IO ()
printString prefix str = if mateDEBUG
  then hPutStr logHandle . (++) prefix $ str
  else return ()


printfJit, printfBb, printfMp, printfCp, printfStr, printfInfo  :: String -> IO ()
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
