SHELL := bash

JAVAC := javac
JAVA_FILES := $(wildcard tests/*.java java/lang/*.java java/io/*.java)
CLASS_FILES := $(JAVA_FILES:.java=.class)
TEST_JAVA_FILES := $(wildcard tests/*.java)
TEST_CLASS_FILES := $(TEST_JAVA_FILES:.java=.test)
HS_FILES := $(wildcard Mate/*.hs)
HS_BOOT := $(wildcard Mate/*.hs-boot)
O_FILES = $(shell ls Mate/*.o) $(wildcard ffi/*.o)
PACKAGES_ := bytestring harpy hs-java
PACKAGES := $(addprefix -package ,$(PACKAGES_))

GHC_OPT := -I. -dynamic -Wall -O0 -fno-warn-unused-do-bind
GHC_LD := -optl-Xlinker -optl-x


.PHONY: all test clean ghci

all: mate $(CLASS_FILES)

%: %.class mate
	./mate $(basename $<)


tests: mate $(TEST_CLASS_FILES)

%.test: %.class mate
	@./tools/openjdktest.sh $(basename $@)


%.class: %.java
	$(JAVAC) $<

ffi/native.o: ffi/native.c
	ghc -Wall -O2 -c $< -o $@

mate: Mate.hs ffi/trap.c $(HS_FILES) $(HS_BOOT) ffi/native.o
	@mkdir -p build/release
	ghc --make $(GHC_OPT) Mate.hs ffi/trap.c -o $@ $(GHC_LD) -outputdir build/release

%.dbg: %.class mate.dbg
	./mate.dbg $(basename $<)

ifeq (${DBGFLAGS},)
DEBUGFLAGS = -DDBG_JIT -DDBG_MP
else
DEBUGFLAGS = ${DBGFLAGS}
endif
mate.dbg: Mate.hs ffi/trap.c $(HS_FILES) $(HS_BOOT) ffi/native.o
	@mkdir -p build/debug/{ffi,Mate,}
	gcc -Wall $(DEBUGFLAGS) -O0 -c ffi/trap.c -o build/debug/ffi/trap.o
	ghc --make $(DEBUGFLAGS) $(GHC_OPT) Mate.hs build/debug/ffi/trap.o -o $@ $(GHC_LD) -outputdir build/debug

clean:
	rm -rf build mate mate.dbg ffi/native.o tests/*.class Mate/*_stub.*

ghci: mate
	ghci $(PACKAGES) $(O_FILES) Mate.hs $(GHC_LD)

tags: mate
	@# @-fforce-recomp, see
	@# http://stackoverflow.com/questions/7137414/how-do-i-force-interpretation-in-hint
	ghc -fforce-recomp -e :ctags $(PACKAGES) $(HS_FILES) $(O_FILES) Mate.hs

hlint:
	@# hlint isn't able to evaluate CPP comments correctly *sigh*
	@cp debug.h debug_tmp.h
	@# so we remove them "by hand", for hlint
	@gcc -E -x c -fpreprocessed -dD -E debug_tmp.h | grep -v 'debug_tmp.h' > debug.h
	@# ignore error code from hlint
	-hlint Mate.hs Mate/
	@mv debug_tmp.h debug.h
