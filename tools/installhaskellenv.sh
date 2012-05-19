#!/bin/bash -x

rm -rf ~/.ghc ~/.cabal
cabal update
cabal install cabal-install --enable-shared
export PATH=~/.cabal/bin:$PATH
cabal update
cabal install missingh --enable-shared
cabal install heap --enable-shared
cabal install plugins --enable-shared

git clone git://wien.tomnetworks.com/disassembler.git
cd disassembler
cabal install --enable-shared
cd ..
rm -rf disassembler

git clone git://wien.tomnetworks.com/harpy.git
cd harpy
cabal install --enable-shared
cd ..
rm -rf harpy

git clone git://wien.tomnetworks.com/hs-java.git
cd hs-java
git checkout -t origin/v0.2
cabal install --enable-shared
cd ..
rm -rf hs-java

echo DONE
