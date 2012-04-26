{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Data.Word
import Text.Printf
import Foreign

foreign import ccall "dynamic"
   code_void :: FunPtr (IO ()) -> (IO ())

main :: IO ()
main = do
  entryPtr <- (mallocBytes 2)
  poke entryPtr (0xc390 :: Word16) -- nop (0x90); ret(0xc3) (little endian order)

  _ <- printf "entry point: 0x%08x\n" ((fromIntegral $ ptrToIntPtr entryPtr) :: Int)
  code_void $ castPtrToFunPtr entryPtr
  putStrLn "welcome back"
