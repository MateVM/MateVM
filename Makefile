all: mate Test.class
	./mate Test.class

%.class: %.java
	javac $<

mate: Mate.hs
	ghc --make -O2 $< -o $@

clean:
	rm -f *.hi *.o mate
