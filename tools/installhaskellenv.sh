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

#hs: i ran into the problem that cabal install does not execute
#my Setup.hs - as workaround i invoke runhaskell Setup.hs etc directly
function gitinstallWithCustomSetup {
	url=$1
	git clone $url hs-boehmgc
	cd hs-boehmgc
        runhaskell Setup.hs configure --user $CABAL_OPT
        runhaskell Setup.hs build 
        runhaskell Setup.hs install
	cd ..
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
cabal install bimap $CABAL_OPT

# cabal install hs-java $CABAL_OPT
gitinstall git://github.com/MateVM/hs-java.git

cabal install disassembler $CABAL_OPT
# cabal install harpy $CABAL_OPT
gitinstall git://github.com/MateVM/harpy.git

gitinstallWithCustomSetup git://github.com/MateVM/hs-boehmgc.git

echo DONE
