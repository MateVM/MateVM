module Mate.RgsOptions(
    HsGcOptions(..)
  , GCOptions ) where

data HsGcOptions = HsGcOptions { 
    blockSize :: Int          -- unit of allocation
  , periodicGc :: Maybe Int   -- whether to perform periodic gc within [ms]
  , activationFillRate :: Float -- fill rate at which gc should perform [%/100]
  , parallelGc :: Bool }
  

-- options for either hs precise gc or boehm weiser reference gc
type GCOptions = Either HsGcOptions ()
