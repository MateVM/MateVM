SHELL := bash

JAVAC := javac
JAVA_FILES := $(wildcard tests/*.java)
CLASS_FILES := $(JAVA_FILES:.java=.class)
HS_FILES := $(wildcard Mate/*.hs)

GHC_OPT := -Wall -O2 -fno-warn-unused-do-bind

all: mate $(CLASS_FILES)

test: mate $(CLASS_FILES)
	./$<

%.class: %.java
	$(JAVAC) $<

trap.o mate: Mate.hs trap.c $(HS_FILES)
	ghc --make $(GHC_OPT) Mate.hs trap.c -o mate

clean:
	rm -f {Mate/,}*.hi {Mate/,}*.o mate tests/*.class

tags: Mate.hs $(HS_FILES) trap.o
	@# @-fforce-recomp, see
	@# http://stackoverflow.com/questions/7137414/how-do-i-force-interpretation-in-hint
	ghc -fforce-recomp -e :ctags $^
