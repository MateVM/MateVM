{-#LANGUAGE ForeignFunctionInterface #-}

module Main where

import qualified Data.Map as M

import Data.Word
import Text.Printf
import Foreign
import Foreign.C.Types

import Data.IORef

import System.Random

foreign import ccall "dynamic"
   code_void :: FunPtr (IO ()) -> (IO ())

foreign import ccall "static sys/mman.h"
  mprotect :: CUInt -> CUInt -> Int -> IO ()

foreign import ccall "static stdlib.h"
  memalign :: CUInt -> CUInt -> IO (Ptr a)


foreign import ccall "wrapper"
  wrap :: (CUInt -> Ptr SigInfo -> Ptr Context -> IO ()) -> IO (FunPtr (CUInt -> Ptr SigInfo -> Ptr Context -> IO ()))

foreign import ccall "prototypes.h"
  registerSignalHandlers2 :: FunPtr (CUInt -> Ptr SigInfo -> Ptr Context -> IO ()) -> IO ()

type SigInfo = ()
type Context = ()              

data MateExecutionCtx = Ctx { compiledMethods :: M.Map Int Int }
emptyCtx :: MateExecutionCtx
emptyCtx = Ctx { compiledMethods = M.empty }

type AppDomain = ()  -- classpath etc

-- add AppDomain to MateExecutionCtx in order to get linear access

runMateKernel :: AppDomain -> IO ()
runMateKernel _ = do    
  compileAndRun


-- use FFI to unpack sigInfo and ctx....
handler mateCtx signal sigInfo ctx = do 
  putStr "handler got me."
  print signal
  putStr "content of code cache: " 
  actualCtx <- readIORef mateCtx
  let methods = compiledMethods actualCtx
  random <- senseless
  print methods
  -- write back new compiled stuff
  writeIORef mateCtx (Ctx {compiledMethods = M.insert random random methods})
  _ <- getChar
  compileAndRun -- tail

main :: IO ()
main = do 

  -- load application context (classpath etc)
  let appDomain = undefined

  ctx <- newIORef emptyCtx

  -- curry context into handler
  actualHandler <- wrap (handler ctx)
  
  -- perform global setup
  registerSignalHandlers2 actualHandler
  
  runMateKernel appDomain


compileAndRun :: IO ()
compileAndRun = do
  entryPtr <- memalign 0x1000 0x2 
  poke entryPtr (0xffff9090 :: Word32) -- SIGILL
  --poke entryPtr (0xc390 :: Word16) -- nop (0x90); ret(0xc3) (little endian order)
  let i_entry = (fromIntegral $ ptrToIntPtr entryPtr) :: Int
  -- 0x7 = PROT_{READ,WRITE,EXEC}
  mprotect (fromIntegral i_entry) 2 0x7
  _ <- printf "entry point: 0x%08x\n" i_entry
  code_void $ castPtrToFunPtr entryPtr
  putStrLn "welcome back"


senseless :: IO Int
senseless = getStdRandom (randomR (1,100))
