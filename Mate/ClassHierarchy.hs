module Mate.ClassHierarchy
  ( isInstanceOf
  , isInstanceOf'
  , addClassEntry
  , addInterfaceEntry
  ) where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Data.List
import Control.Monad

import Foreign hiding (unsafePerformIO)
import System.IO.Unsafe
import Data.IORef

import Mate.NativeSizes
import Mate.ClassPool


data Class
  = Class NativeWord [B.ByteString]
  | JavaLangObject

type ClassHier = M.Map NativeWord Class
classHier :: IORef ClassHier
{-# NOINLINE classHier #-}
classHier = unsafePerformIO $ newIORef M.empty

type InterfaceHier = M.Map B.ByteString [B.ByteString]
interfaceHier :: IORef InterfaceHier
{-# NOINLINE interfaceHier #-}
interfaceHier = unsafePerformIO $ newIORef M.empty

readClass :: IO ClassHier
readClass = readIORef classHier
readInterface :: IO InterfaceHier
readInterface = readIORef interfaceHier

writeClass :: ClassHier -> IO ()
writeClass = writeIORef classHier
writeInterface :: InterfaceHier -> IO ()
writeInterface = writeIORef interfaceHier


isInstanceOf' :: NativeWord -> B.ByteString -> IO Bool
isInstanceOf' obj_mtable classname = do
  ch <- readClass
  ih <- readInterface
  if M.member classname ih
    then do -- interface check
      let ai = allInterfaces obj_mtable ch
      return $ checkInterfaces ai classname ih
    else do -- class check
      class_mtable <- getMethodTable classname
      return $ checkInstance obj_mtable class_mtable ch

isInstanceOf :: NativeWord -> B.ByteString -> IO Bool
isInstanceOf 0 _ = return False
isInstanceOf obj classname = do
  obj_mtable <- peek (intPtrToPtr . fromIntegral $ obj)
  isInstanceOf' obj_mtable classname

allInterfaces :: NativeWord -> ClassHier -> [B.ByteString]
allInterfaces obj_mtable ch =
  case ch M.! obj_mtable of
    JavaLangObject -> []
    Class superclass ifaces -> ifaces ++ allInterfaces superclass ch

checkInterfaces :: [B.ByteString] -> B.ByteString -> InterfaceHier -> Bool
checkInterfaces [] _ _ = False
checkInterfaces ifaces target ih
  | target `elem` ifaces = True
  | otherwise = checkInterfaces (nextifaces \\ ifaces) target ih
    where
      nextifaces = concatMap (\x -> ih M.! x) ifaces

checkInstance :: NativeWord -> NativeWord -> ClassHier -> Bool
checkInstance obj cl_mtable ch
  | obj == cl_mtable = True
  | otherwise =
      case ch M.! obj of
        Class super _ -> checkInstance super cl_mtable ch
        JavaLangObject -> False

addClassEntry :: NativeWord -> NativeWord -> [B.ByteString] -> IO ()
addClassEntry mtable 0 _ = do
  ch <- readClass
  writeClass (M.insert mtable JavaLangObject ch)
addClassEntry mtable super_mtable ifaces = do
  ch <- readClass
  unless (M.member super_mtable ch) $ error "classhierarchy: superclass should be in hierarchy!"
  writeClass (M.insert mtable (Class super_mtable ifaces) ch)

addInterfaceEntry :: B.ByteString -> [B.ByteString] -> IO ()
addInterfaceEntry iface super_ifaces = do
  ch <- readInterface
  -- TODO: check super if's
  writeInterface (M.insert iface super_ifaces ch)
