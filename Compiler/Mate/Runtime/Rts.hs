module Compiler.Mate.Runtime.Rts (loadLibrary) where

foreign export ccall loadLibrary :: IO ()
loadLibrary :: IO ()
loadLibrary = print "load lib" 
