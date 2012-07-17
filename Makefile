SHELL := bash

JAVAC := javac
JAVA_FILES := $(wildcard jmate/lang/*.java jmate/io/*.java java/lang/*.java java/io/*.java)
CLASS_FILES := $(JAVA_FILES:.java=.class)
TEST_JAVA_FILES := $(wildcard tests/*.java)
TEST_CLASS_FILES := $(TEST_JAVA_FILES:.java=.test)
HS_FILES := $(wildcard Mate/*.hs)
HS_BOOT := $(wildcard Mate/*.hs-boot)
BUILD := build
B_RELEASE := $(BUILD)/release
B_STATIC := $(BUILD)/static
B_DEBUG := $(BUILD)/release
O_STATIC_FILES = $(shell ls $(B_STATIC)/Mate/*.o) $(wildcard $(B_STATIC)/ffi/*.o)
PACKAGES_ := bytestring harpy hs-java plugins
PACKAGES := $(addprefix -package ,$(PACKAGES_))

GHC_OPT := -I. -Wall -O0 -fno-warn-unused-do-bind
GHC_LD := -optl-Xlinker -optl-x


.PHONY: all test clean ghci

all: mate

%: %.class mate
	./mate $(basename $<)


tests: mate $(TEST_JAVA_FILES:.java=.class) $(TEST_CLASS_FILES)

CALLF = $(basename $@).call
testcase = ./tools/openjdktest.sh "$(1) $(basename $@)"
%.test: %.class mate
	@if [ -f $(CALLF) ]; \
		then $(call testcase,`cat $(CALLF)`); \
		else $(call testcase, ); fi

COMPILEF = $(basename $@).compile
%.class: %.java
	@if [ -f $(COMPILEF) ]; \
		then $(SHELL) $(COMPILEF); \
		else $(JAVAC) $(JAVA_FILES) $<; fi
	@echo "JAVAC $<"

ffi/native.o: ffi/native.c
	ghc -Wall -O2 -c $< -o $@

GHCCALL = ghc --make $(GHC_OPT) Mate.hs ffi/trap.c -o $@ $(GHC_LD) -outputdir
mate: Mate.hs ffi/trap.c $(HS_FILES) $(HS_BOOT) ffi/native.o $(CLASS_FILES)
	@mkdir -p $(B_RELEASE)
	$(GHCCALL) $(B_RELEASE) -dynamic

mate.static: Mate.hs ffi/trap.c $(HS_FILES) $(HS_BOOT) ffi/native.o $(CLASS_FILES)
	@mkdir -p $(B_STATIC)
	$(GHCCALL) $(B_STATIC) -static

%.dbg: %.class mate.dbg
	./mate.dbg $(basename $<)

ifeq (${DBGFLAGS},)
DEBUGFLAGS = -DDBG_JIT -DDBG_MP
else
DEBUGFLAGS = ${DBGFLAGS}
endif
mate.dbg: Mate.hs ffi/trap.c $(HS_FILES) $(HS_BOOT) ffi/native.o
	@mkdir -p $(B_DEBUG)/{ffi,Mate,}
	gcc -Wall $(DEBUGFLAGS) -O0 -c ffi/trap.c -o $(B_DEBUG)/ffi/trap.o
	ghc --make $(DEBUGFLAGS) $(GHC_OPT) Mate.hs $(B_DEBUG)/ffi/trap.o -o $@ $(GHC_LD) -outputdir $(B_DEBUG)

clean:
	rm -rf $(BUILD) mate mate.static mate.dbg tags ffi/native.o \
		tests/*.class Mate/*_stub.* \
		jmate/lang/*.class jmate/io/*.class java/io/*.class \
		java/lang/{Integer,Character,String,System}.class

ghci: mate.static
	ghci -I. $(PACKAGES) $(O_STATIC_FILES) -outputdir $(B_STATIC) Mate.hs

tags: mate
	@# @-fforce-recomp, see
	@# http://stackoverflow.com/questions/7137414/how-do-i-force-interpretation-in-hint
	@# @-fobject-code: force to generate native code (necessary for ffi stuff)
	ghc -I. -fforce-recomp -fobject-code $(PACKAGES) Mate.hs $(O_STATIC_FILES) -outputdir $(B_STATIC) -e :ctags

hlint:
	@# hlint isn't able to evaluate CPP comments correctly *sigh*
	@cp debug.h debug_tmp.h
	@# so we remove them "by hand", for hlint
	@gcc -E -x c -fpreprocessed -dD -E debug_tmp.h | grep -v 'debug_tmp.h' > debug.h
	@# ignore error code from hlint
	-hlint Mate.hs Mate/
	@mv debug_tmp.h debug.h
