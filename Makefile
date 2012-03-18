all: mate Test.class
	./mate Test.class

%.class: %.java
	javac $<

mate: Mate.hs trap.c
	ghc --make -Wall -O2 $^ -o $@

clean:
	rm -f *.hi *.o mate
