SHELL := bash

JAVAC := javac
JAVA_FILES := $(wildcard tests/*.java java/lang/*.java java/io/*.java)
CLASS_FILES := $(JAVA_FILES:.java=.class)
TEST_JAVA_FILES := $(wildcard tests/*.java)
TEST_CLASS_FILES := $(TEST_JAVA_FILES:.java=)
HS_FILES := $(wildcard Mate/*.hs)
HS_BOOT := $(wildcard Mate/*.hs-boot)
O_FILES = $(shell ls Mate/*.o) $(wildcard ffi/*.o)
PACKAGES_ := bytestring harpy hs-java
PACKAGES := $(addprefix -package ,$(PACKAGES_))

GHC_OPT := -dynamic -Wall -O0 -fno-warn-unused-do-bind
GHC_LD := -optl-Xlinker -optl-x


.PHONY: all test clean ghci

all: mate $(CLASS_FILES)

test: $(TEST_CLASS_FILES)

%: %.class mate
	@./tools/openjdktest.sh $@

%.class: %.java
	$(JAVAC) $<

ffi/native.o: ffi/native.c
	ghc -Wall -O2 -c $< -o $@

mate: Mate.hs ffi/trap.c $(HS_FILES) $(HS_BOOT) ffi/native.o
	ghc --make $(GHC_OPT) Mate.hs ffi/trap.c -o $@ $(GHC_LD)

clean:
	rm -f {Mate/,}*.hi {Mate/,ffi/,}*.o Mate/*.{hi,o}-boot mate tests/*.class

ghci: mate
	ghci $(PACKAGES) $(O_FILES) Mate.hs $(GHC_LD)

tags: mate
	@# @-fforce-recomp, see
	@# http://stackoverflow.com/questions/7137414/how-do-i-force-interpretation-in-hint
	ghc -fforce-recomp -e :ctags $(PACKAGES) $(HS_FILES) $(O_FILES) Mate.hs
