Adventures in RTS - or - Status of the GCs available in MateVM
==============================================================

## Story

As reference GC we implemented a simple wrapper[1] for BoehmGC[2]. If something
goes wrong (tests fail) it is always a good idea to witch to BoehmGC - 
to be sure the GC does not mess up something. 
The C binding is split into a separate cabal package because the build process is not as
straightforward as for MateVM so we decided to keep this stuff out.

Given this (ground zero) our mission was to show a way to implement parts of the
runtime system in Haskell as well. It turned out to work better than expected :)

First we implemented a simple copying gc sheme:
* two fixed blocks of memory
* evacuate objects alive into the other area 
* switch to and from space and continue execution
* evacuate objects again into the oter memory area...
* Large objects are in separate space and get freed immediately (i.e. not moved)
This is realized in TwoSpaceAllocator.hs

A slightly improved memory manager is available in BlockAllocation.hs.
Active memory is divided into (power of two large) blocks - whenever a block
is conceptually freed it is put into a free list. Another allocation may
use this memory again. In principle it would be possible resize the underlying
memory buffer depending on the VMs needs. However this is currently not implemented
but should be quite straightforward.

As next step we introduced a generational GC (inspired by[3]). The assumption is,
that many objects live shortly. Gen0 collections take place quite frequently,
higher collections less frequent. This is sketched in GenerationalGC.hs.

## Future Work

GC.hs provides a nice and abstract way to traverse object trees (which are in fact nothing
but (Ptr a)'s). RefObj typeclass provides this abstract interface (mostly in IO). 
JavaObjectsGC.hs defines a RefObj instance for Ptr. The implementations mentioned above
use a rich set of simple functions in GC.hs which operate on RefObjs. There exist efficient
implementations of Obj-Tree traversals and so on. However TwoSpaceAllocator and GenerationalGC
often use rather inefficient entry points - this is artifact of development and debugging.
That is the main reason all implemented GCs (Except boehm) are *very* inefficient. 
If GC does not take place very often this is not a big issue - nevertheless - it sucks.
Here is definitely work to do...

Other issues are:
* rarely already patched (moved) objects pop up in gc points. This may be due to inproper
patching of the object tree or something else. GenerationalGC implements a stupid
hack to overcome this issue - free blocks are queued, stay inactive but accessible. This
way old references are still valid and get hopefully patched within the next GC invocation.
Empirical analysis showed that these invalid references show up immediately after previous GC
and vanish after the next one. However, this is just intution and sucks as well.
* Builtin efficiency problems - I suspecd many many functions to be unnecessarily lazy. If
the previously mentioned issued are solved it is time to do awesome memory profiling.


To sum up there is still much to do, but we found it is possible to implement parts of the
RuntimeSystem in haskell in a quite nice and abstract way.

[1] https://github.com/MateVM/hs-boehmgc
[2] http://www.hpl.hp.com/personal/Hans_Boehm/gc/
[3] http://dl.acm.org/citation.cfm?id=808261 
