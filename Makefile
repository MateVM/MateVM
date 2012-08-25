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
B_DEBUG := $(BUILD)/debug
PACKAGES_ := bytestring harpy hs-java plugins
PACKAGES := $(addprefix -package ,$(PACKAGES_))


GHC_CPP := -DARCH_X86

GHC_OPT  = -I. -O0 -Wall -fno-warn-unused-do-bind -fwarn-tabs
# TODO: define this in cabal... (see cpu package @ hackage)
# see *.gdb target. also useful for profiling (-p at call)
GHC_OPT += -rtsopts # -prof -auto-all
GHC_OPT += $(GHC_CPP)

# dunno anymore? some linker stuff regarding GHCi
GHC_LD := -optl-Xlinker -optl-x


.PHONY: all tests clean ghci hlint

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

runtime: jmate/lang/MateRuntime.java
	javac jmate/lang/MateRuntime.java
	javah  -o rts/mock/jmate_lang_MateRuntime.h jmate.lang.MateRuntime
	gcc -shared -fPIC -I$(JAVA_HOME)/include rts/mock/jmate_lang_MateRuntime.c -I./rts/mock -o rts/mock/libMateRuntime.so 

GHCCALL = ghc --make $(GHC_OPT) Mate.hs ffi/trap.c -o $@ $(GHC_LD) -outputdir
mate: Mate.hs ffi/trap.c $(HS_FILES) $(HS_BOOT) ffi/native.o $(CLASS_FILES)
	@mkdir -p $(B_RELEASE)
	$(GHCCALL) $(B_RELEASE) -dynamic

mate.static: Mate.hs ffi/trap.c $(HS_FILES) $(HS_BOOT) ffi/native.o $(CLASS_FILES)
	@mkdir -p $(B_STATIC)
	$(GHCCALL) $(B_STATIC) -static

%.gdb: %.class mate
	gdb -x .gdbcmds -q --args mate $(basename $<) +RTS -V0 --install-signal-handlers=no

clean:
	rm -rf $(BUILD) mate mate.static ffi/native.o \
		tests/*.class Mate/*_stub.* \
		jmate/lang/*.class jmate/io/*.class java/io/*.class \
		java/lang/{Integer,Character,String,System}.class \
		scratch/*.class

ghci: mate.static
	ghci -I. $(PACKAGES) -outputdir $(B_STATIC) Mate.hs $(GHC_CPP)

tags: mate.static
	@# @-fforce-recomp, see
	@# http://stackoverflow.com/questions/7137414/how-do-i-force-interpretation-in-hint
	@# @-fobject-code: force to generate native code (necessary for ffi stuff)
	ghc -I. -fforce-recomp -fobject-code $(PACKAGES) Mate.hs -outputdir $(B_STATIC) -e :ctags $(GHC_CPP)

hlint:
	hlint Mate.hs Mate/

scratch: mate $(wildcard jmate/lang/*.java) scratch/GCTest.java
	javac $(wildcard jmate/lang/*.java)
	javac scratch/GCTest.java
	./mate scratch.GCTest  
