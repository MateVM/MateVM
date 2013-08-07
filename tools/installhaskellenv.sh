#!/bin/bash -x

CABAL_OPT="--enable-shared -p"
CABAL_OPT_NEW="--enable-shared -p -j"

function gitinstall {
	url=$1
	git clone $url tmprepo
	cd tmprepo
	cabal install $CABAL_OPT_NEW
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
cabal install mtl random $CABAL_OPT
cabal install cabal-install $CABAL_OPT
export PATH=~/.cabal/bin:$PATH
cabal update
cabal install missingh heap plugins split bimap disassembler intervalmap $CABAL_OPT_NEW

# explicitly install not so recent libzip version, since on the suggested
# system (ubuntu 12.04) libzip-0.11 is not available.
cabal install libzip-0.10.2 $CABAL_OPT

# cabal install hs-java $CABAL_OPT_NEW
gitinstall git://github.com/MateVM/hs-java.git

# cabal install harpy $CABAL_OPT
gitinstall git://github.com/MateVM/harpy.git

# cabal install hoopl $CABAL_OPT
gitinstall git://github.com/MateVM/hoopl.git
# gitinstall https://github.com/ghc/packages-hoopl


gitinstallWithCustomSetup git://github.com/MateVM/hs-boehmgc.git

echo DONE
