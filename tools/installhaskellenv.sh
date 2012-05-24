#!/bin/bash -x

CABAL_OPT="--enable-shared -p"
rm -rf ~/.ghc ~/.cabal
cabal update
cabal install cabal-install $CABAL_OPT
export PATH=~/.cabal/bin:$PATH
cabal update
cabal install missingh $CABAL_OPT
cabal install heap $CABAL_OPT
cabal install plugins $CABAL_OPT
cabal install split $CABAL_OPT

git clone git://wien.tomnetworks.com/hs-java.git
cd hs-java
cabal install $CABAL_OPT
cd ..
rm -rf hs-java

cabal install disassembler $CABAL_OPT
cabal install harpy $CABAL_OPT

echo DONE
