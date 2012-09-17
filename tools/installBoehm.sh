#!/bin/bash -x
git clone git://github.com/MateVM/hs-boehmgc.git hs-boehmgc
cd hs-boehmgc
runhaskell Setup.hs configure --user $CABAL_OPT
runhaskell Setup.hs build 
runhaskell Setup.hs install
cd ..
echo DONE
