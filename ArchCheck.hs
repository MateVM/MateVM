module Main where

import Data.Elf
import System.Environment
import qualified Data.ByteString as B
import Control.Monad

main :: IO ()
main = liftM parseElf (liftM head getArgs >>= B.readFile) >>= 
                   \elf -> if elfClass elf == ELFCLASS64 then print 64 else print 32
