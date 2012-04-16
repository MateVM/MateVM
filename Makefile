SHELL := bash

JAVAC := javac
JAVA_FILES := $(wildcard tests/*.java)
CLASS_FILES := $(JAVA_FILES:.java=.class)
HS_FILES := $(wildcard Mate/*.hs)
O_FILES = $(shell ls Mate/*.o) $(wildcard ffi/*.o)
PACKAGES_ := bytestring harpy hs-java
PACKAGES := $(addprefix -package ,$(PACKAGES_))

GHC_OPT := -Wall -O0 -fno-warn-unused-do-bind
GHC_LD := -optl-Xlinker -optl-x


.PHONY: all test clean ghci

all: mate $(CLASS_FILES)

test: mate $(CLASS_FILES)
	./$< tests/Fib.class

%.class: %.java
	$(JAVAC) $<

mate: Mate.hs ffi/trap.c $(HS_FILES)
	ghc --make $(GHC_OPT) Mate.hs ffi/trap.c -o $@ $(GHC_LD)

clean:
	rm -f {Mate/,}*.hi {Mate/,ffi/,}*.o mate tests/*.class

ghci: mate
	ghci $(PACKAGES) $(O_FILES) Mate.hs $(GHC_LD)

tags: mate
	@# @-fforce-recomp, see
	@# http://stackoverflow.com/questions/7137414/how-do-i-force-interpretation-in-hint
	ghc -fforce-recomp -e :ctags $(PACKAGES) $(HS_FILES) $(O_FILES) Mate.hs
