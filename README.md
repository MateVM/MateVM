MateVM is a Java JIT compiler written in Haskell, using already existing
libaries, namly [Harpy](http://hackage.haskell.org/package/harpy) and
[hs-java](http://hackage.haskell.org/package/hs-java).

MateVM is i686 (i.e. 32-Bit) only at the moment.

Visit us at `#MateVM` on [OFTC](http://www.oftc.net/oftc/)!

We use `hs-java` to parse Java Classfiles in order to get a Java Bytecode
representation. Afterwards we perform a basic-block analysis and generate a
control-flow-graph (CFG). We apply several program-analyses, transformations
and optimizations. Given the annotated CFG we emit native code for a method with
`Harpy` (i686) in an on demand manner.

At the moment we try to minimize effort, by focusing on essential features of
the JVM, for example there is no support for threads yet.

## Dependencies
Install GHC via your distro manager. Make sure you install it with shared
libaries. On Ubuntu 12.04 LTS that is:

    $ sudo apt-get install ghc-dynamic

For an "easy" installation of the remaining packages, you can use
the script `./tools/installhaskellenv.sh`.

**PLEASE NOTE**: it deletes your `~/.cabal` and `~/.ghc directory` first in
order to get a fresh cabal install!
