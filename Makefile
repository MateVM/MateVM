SHELL := bash

JAVAC := javac
JAVA_FILES := $(wildcard tests/*.java)
CLASS_FILES := $(JAVA_FILES:.java=.class)
HS_FILES := $(wildcard Mate/*.hs)
O_FILES = $(shell ls Mate/*.o) $(wildcard ffi/*.o)
PACKAGES_ := bytestring harpy hs-java
PACKAGES := $(addprefix -package ,$(PACKAGES_))

GHC_OPT := -dynamic -Wall -O0 -fno-warn-unused-do-bind
GHC_LD := -optl-Xlinker -optl-x


.PHONY: all test clean ghci

all: mate $(CLASS_FILES)

test: mate $(CLASS_FILES)
	./$< tests/Fib | grep mainresult
	@printf "should be:  0x%08x\n" 0x09de8d6d
	./$< tests/Fac | grep mainresult
	@printf "should be:  0x%08x\n" 0x58980
	./$< tests/ArgumentPassing1 | grep mainresult
	@printf "should be:  0x%08x\n" 0x92
	@printf "should be:  0x%08x\n" $$(((0 - 0x1337) & 0xffffffff))
	./$< tests/DifferentClass1 | grep mainresult
	@printf "should be:  0x%08x\n" 8
	@printf "should be:  0x%08x\n" 13
	./$< tests/Native1 | egrep -i -e '^printsomething: '
	./$< tests/Static1 | grep mainresult
	@printf "should be:  0x%08x\n" 0x33
	./$< tests/Static2 | grep mainresult
	@printf "should be:  0x%08x\n" 0x55
	./$< tests/Static3 | grep mainresult
	@printf "should be:  0x%08x\n" 0x6dd

%.class: %.java
	$(JAVAC) $<

ffi/native.o: ffi/native.c
	ghc -Wall -O2 -c $< -o $@

mate: Mate.hs ffi/trap.c $(HS_FILES) ffi/native.o
	ghc --make $(GHC_OPT) Mate.hs ffi/trap.c -o $@ $(GHC_LD)

clean:
	rm -f {Mate/,}*.hi {Mate/,ffi/,}*.o mate tests/*.class

ghci: mate
	ghci $(PACKAGES) $(O_FILES) Mate.hs $(GHC_LD)

tags: mate
	@# @-fforce-recomp, see
	@# http://stackoverflow.com/questions/7137414/how-do-i-force-interpretation-in-hint
	ghc -fforce-recomp -e :ctags $(PACKAGES) $(HS_FILES) $(O_FILES) Mate.hs
