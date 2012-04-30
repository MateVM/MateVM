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

tests: mate $(TEST_CLASS_FILES)

%: %.class mate
	@./tools/openjdktest.sh $@

%.class: %.java
	$(JAVAC) $<

ffi/native.o: ffi/native.c
	ghc -Wall -O2 -c $< -o $@

mate: Mate.hs ffi/trap.c $(HS_FILES) $(HS_BOOT) ffi/native.o
	@mkdir -p build/release
	ghc --make $(GHC_OPT) Mate.hs ffi/trap.c -o $@ $(GHC_LD) -outputdir build/release

%.dbg: %.class mate.dbg
	./mate.dbg $(basename $<)

mate.dbg: Mate.hs ffi/trap.c $(HS_FILES) $(HS_BOOT) ffi/native.o
	@mkdir -p build/debug/{ffi,Mate,}
	gcc -Wall -DDEBUG -O0 -c ffi/trap.c -o build/debug/ffi/trap.o
	ghc --make -DDEBUG $(GHC_OPT) Mate.hs build/debug/ffi/trap.o -o $@ $(GHC_LD) -outputdir build/debug

clean:
	rm -rf build mate mate.dbg ffi/native.o tests/*.class

ghci: mate
	ghci $(PACKAGES) $(O_FILES) Mate.hs $(GHC_LD)

tags: mate
	@# @-fforce-recomp, see
	@# http://stackoverflow.com/questions/7137414/how-do-i-force-interpretation-in-hint
	ghc -fforce-recomp -e :ctags $(PACKAGES) $(HS_FILES) $(O_FILES) Mate.hs
