JAVAC = javac
JAVA_FILES := $(wildcard tests/*.java)
CLASS_FILES := $(JAVA_FILES:.java=.class)

all: mate $(CLASS_FILES)
	./mate tests/Test.class

%.class: %.java
	javac $<

mate: Mate.hs ./src/Utilities.hs trap.c
	ghc --make -Wall -O2 $^ -o $@

clean:
	rm -f *.hi *.o mate src/*.class
