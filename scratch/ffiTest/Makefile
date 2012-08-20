all: mate_support.c trapTest.hs
	ghc -c mate_support.c
	ghc --make trapTest.hs mate_support.o

ghci: all
	ghci -package containers -package random trapTest.o mate_support.o trapTest_stub.o trapTest.hs

clean:
	rm -f *.o *.hi trapTest trapTest_stub.c trapTest_stub.h
