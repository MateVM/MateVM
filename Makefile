JAVAC = javac
JAVA_FILES := $(wildcard tests/*.java)
CLASS_FILES := $(JAVA_FILES:.java=.class)

all: mate $(CLASS_FILES)

test: mate
	./mate tests/Test.class

%.class: %.java
	javac $<

trap.o mate: Mate.hs ./src/Utilities.hs trap.c
	ghc --make -Wall -O2 $^ -o mate

clean:
	rm -f *.hi *.o mate src/*.class

tags: Mate.hs src/Utilities.hs trap.o
	@# @-fforce-recomp, see
	@# http://stackoverflow.com/questions/7137414/how-do-i-force-interpretation-in-hint
	ghc -fforce-recomp -e :ctags $^
