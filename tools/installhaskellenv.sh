#!/bin/bash -x

CABAL_OPT="--enable-shared -p"

function gitinstall {
	url=$1
	git clone $url tmprepo
	cd tmprepo
	cabal install $CABAL_OPT
	cd ..
	rm -rf tmprepo
}

rm -rf ~/.ghc ~/.cabal
cabal update
cabal install cabal-install $CABAL_OPT
export PATH=~/.cabal/bin:$PATH
cabal update
cabal install missingh $CABAL_OPT
cabal install heap $CABAL_OPT
cabal install plugins $CABAL_OPT
cabal install split $CABAL_OPT

# cabal install hs-java $CABAL_OPT
gitinstall git://wien.tomnetworks.com/hs-java.git

cabal install disassembler $CABAL_OPT
# cabal install harpy $CABAL_OPT
gitinstall git://wien.tomnetworks.com/harpy.git

echo DONE
