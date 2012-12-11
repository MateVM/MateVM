{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Compiler.Mate.Runtime.NativeMethods
  ( nativeAddr
  ) where

import qualified Data.ByteString.Lazy as B
import Data.String.Utils
import System.Plugins
import Foreign.C.String

import Control.Monad

import Foreign
import Foreign.C.Types

import JVM.ClassFile

import Compiler.Mate.Runtime.JavaObjects()
import Compiler.Mate.Debug
import Compiler.Mate.Backend.NativeSizes


foreign import ccall "&printMemoryUsage"
  printMemoryUsageAddr :: FunPtr (IO ())

foreign import ccall "&printGCStats"
  printGCStatsAddr :: FunPtr (IO ())

foreign import ccall "&cloneObject"
  cloneObjectAddr :: FunPtr (CPtrdiff -> IO CPtrdiff)

foreign import ccall "&printf0"
  printf0Addr :: FunPtr (CPtrdiff -> IO CInt)
foreign import ccall "&printf1"
  printf1Addr :: FunPtr (CPtrdiff -> CPtrdiff -> IO CInt)
foreign import ccall "&printf2"
  printf2Addr :: FunPtr (CPtrdiff -> CPtrdiff -> CPtrdiff -> IO CInt)
foreign import ccall "&printf3"
  printf3Addr :: FunPtr (CPtrdiff -> CPtrdiff -> CPtrdiff -> CPtrdiff -> IO CInt)
foreign import ccall "&printf4"
  printf4Addr :: FunPtr (CPtrdiff -> CPtrdiff -> CPtrdiff -> CPtrdiff -> CPtrdiff -> IO CInt)
foreign import ccall "&printf5"
  printf5Addr :: FunPtr (CPtrdiff -> CPtrdiff -> CPtrdiff -> CPtrdiff -> CPtrdiff -> CPtrdiff -> IO CInt)


funPtrToAddr :: Num b => FunPtr a -> b
funPtrToAddr = fromIntegral . ptrToIntPtr . castFunPtrToPtr

nativeAddr :: Num a => String -> String -> String -> IO a
nativeAddr scm smethod sig = do
  let ret = return . funPtrToAddr
  printfMp $ printf "native-call: %s: %s / %s\n" scm smethod sig
  case scm of
    "jmate/lang/MateRuntime" ->
      case smethod of
        "printGCStats" -> ret printGCStatsAddr
        "printMemoryUsage" -> ret printMemoryUsageAddr
        _ -> error $ "native-call: " ++ smethod ++ " @ " ++ scm ++ " not found."
    "java/lang/VMObject" ->
      case smethod of
        "clone" -> ret cloneObjectAddr
        _ -> error $ "native-call: " ++ smethod ++ " @ " ++ scm ++ " not found."
    "jmate/io/PrintStream" ->
      case smethod of
        "printf_0" -> ret printf0Addr
        "printf_1" -> ret printf1Addr
        "printf_2" -> ret printf2Addr
        "printf_3" -> ret printf3Addr
        "printf_4" -> ret printf4Addr
        "printf_5" -> ret printf5Addr
        _ -> error $ "native-call: " ++ smethod ++ " @ " ++ scm ++ " not found."
    _ -> do
          -- TODO(bernhard): cleaner please... *do'h*
          let sym1 = replace "/" "_" scm
              parenth = replace "(" "_" $ replace ")" "_" sig
              sym2 = replace ";" "_" $ replace "/" "_" parenth
              symbol = sym1 ++ "__" ++ smethod ++ "__" ++ sym2
          printfMp $ printf "native-call: symbol: %s\n" symbol
          nf <- loadNativeFunction symbol
          return $ fromIntegral nf

-- TODO(bernhard): UBERHAX.  ghc patch?
foreign import ccall safe "lookupSymbol"
   c_lookupSymbol :: CString -> IO (Ptr a)

loadNativeFunction :: String -> IO NativeWord
loadNativeFunction sym = do
  _ <- loadRawObject "ffi/native.o"
  -- TODO(bernhard): WTF
  resolveObjs (return ())
  ptr <- withCString sym c_lookupSymbol
  if ptr == nullPtr
    then error $ "dyn. loading of \"" ++ sym ++ "\" failed."
    else return $ fromIntegral $ ptrToIntPtr ptr


type JavaObject = CPtrdiff

fromJavaString :: JavaObject -> IO String
fromJavaString objaddr = do
  straddr <- peek $ intPtrToPtr (fromIntegral $ objaddr + 0x8) :: IO CPtrdiff
  len <-     peek $ intPtrToPtr (fromIntegral $ objaddr + 0xc) :: IO CPtrdiff
  let rptr = intPtrToPtr (fromIntegral $ straddr + 0xc) :: Ptr CPtrdiff
  vals <- forM [0..(len - 1)] $ \i ->
    peek (plusPtr rptr (fromIntegral i)) :: IO Word8
  return $ toString $ B.pack vals

fromJavaInteger :: JavaObject -> IO Int32
fromJavaInteger objaddr = peek $ intPtrToPtr $ fromIntegral $ objaddr + 0x8

foreign export ccall printf0 :: JavaObject -> IO CInt
printf0 :: JavaObject -> IO CInt
printf0 fmt = do
  hfmt <- fromJavaString fmt
  printf hfmt
  return 0

foreign export ccall printf1 :: JavaObject -> JavaObject -> IO CInt
printf1 :: JavaObject -> JavaObject -> IO CInt
printf1 fmt i1 = do
  hfmt <- fromJavaString fmt
  hi1 <- fromJavaInteger i1
  printf hfmt hi1
  return 0

foreign export ccall printf2 :: JavaObject -> JavaObject -> JavaObject -> IO CInt
printf2 :: JavaObject -> JavaObject -> JavaObject -> IO CInt
printf2 fmt i1 i2 = do
  hfmt <- fromJavaString fmt
  hi1 <- fromJavaInteger i1
  hi2 <- fromJavaInteger i2
  printf hfmt hi1 hi2
  return 0

foreign export ccall printf3 :: JavaObject -> JavaObject -> JavaObject -> JavaObject -> IO CInt
printf3 :: JavaObject -> JavaObject -> JavaObject -> JavaObject -> IO CInt
printf3 fmt i1 i2 i3 = do
  hfmt <- fromJavaString fmt
  hi1 <- fromJavaInteger i1
  hi2 <- fromJavaInteger i2
  hi3 <- fromJavaInteger i3
  printf hfmt hi1 hi2 hi3
  return 0

foreign export ccall printf4 :: JavaObject -> JavaObject -> JavaObject -> JavaObject -> JavaObject -> IO CInt
printf4 :: JavaObject -> JavaObject -> JavaObject -> JavaObject -> JavaObject -> IO CInt
printf4 fmt i1 i2 i3 i4 = do
  hfmt <- fromJavaString fmt
  hi1 <- fromJavaInteger i1
  hi2 <- fromJavaInteger i2
  hi3 <- fromJavaInteger i3
  hi4 <- fromJavaInteger i4
  printf hfmt hi1 hi2 hi3 hi4
  return 0

foreign export ccall printf5 :: JavaObject -> JavaObject -> JavaObject -> JavaObject -> JavaObject -> JavaObject -> IO CInt
printf5 :: JavaObject -> JavaObject -> JavaObject -> JavaObject -> JavaObject -> JavaObject -> IO CInt
printf5 fmt i1 i2 i3 i4 i5 = do
  hfmt <- fromJavaString fmt
  hi1 <- fromJavaInteger i1
  hi2 <- fromJavaInteger i2
  hi3 <- fromJavaInteger i3
  hi4 <- fromJavaInteger i4
  hi5 <- fromJavaInteger i5
  printf hfmt hi1 hi2 hi3 hi4 hi5
  return 0
