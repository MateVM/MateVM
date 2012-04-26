SHELL := bash

JAVAC := javac
JAVA_FILES := $(wildcard tests/*.java java/lang/*.java java/io/*.java)
CLASS_FILES := $(JAVA_FILES:.java=.class)
HS_FILES := $(wildcard Mate/*.hs)
HS_BOOT := $(wildcard Mate/*.hs-boot)
O_FILES = $(shell ls Mate/*.o) $(wildcard ffi/*.o)
PACKAGES_ := bytestring harpy hs-java
PACKAGES := $(addprefix -package ,$(PACKAGES_))

GHC_OPT := -dynamic -Wall -O0 -fno-warn-unused-do-bind
GHC_LD := -optl-Xlinker -optl-x


.PHONY: all test clean ghci

all: mate $(CLASS_FILES)

test: mate $(CLASS_FILES)
	./$< tests/While | grep -e "^result"
	@printf "should be:  0x%08x 0x%08x\n" 0x19 0x19
	./$< tests/Fib | grep -e "^result"
	@printf "should be:  0x%08x\n" 0x2ac2
	./$< tests/Fac | grep -e "^result"
	@printf "should be:  0x%08x\n" 0x63e1a
	./$< tests/ArgumentPassing1 | grep -e "^result"
	@printf "should be:  0x%08x 0x%08x\n" 0x92 $$(((0 - 0x1337) & 0xffffffff))
	./$< tests/DifferentClass1 | grep -e "^result"
	@printf "should be:  0x%08x 0x%08x\n" 8 13
	./$< tests/Static1 | grep -e "^result"
	@printf "should be:  0x%08x\n" 0x33
	./$< tests/Static2 | grep -e "^result"
	@printf "should be:  0x%08x\n" 0x55
	./$< tests/Static3 | grep -e "^result"
	@printf "should be:  0x%08x\n" 0x6dd
	./$< tests/Static4 | grep -e "^result"
	@printf "should be:  0x%08x 0x%08x\n" 0x33 0x77
	./$< tests/Static5 | grep -e "^result"
	@printf "should be:  0x%08x 0x%08x\n" 0x33 0x33
	./$< tests/Static6 | grep -e "^result"
	@printf "should be:  0x%08x\n" 0x33
	./$< tests/Static7 | grep -e "^result"
	@printf "should be:  0x%08x\n" $$((0x1337 + 0x555))
	./$< tests/Static8 | grep -e "^result"
	@printf "should be:  0x%08x 0x%08x\n" 0x33 $$((0x1337 + 0x555))
	./$< tests/CallConv1 | grep -e "^result"
	@printf "should be:  0x%08x\n" 0x1337
	./$< tests/CallConv2 | grep -e "^result"
	@printf "should be:  0x%08x\n" 0x1337
	./$< tests/CallConv3 | grep -e "^result"
	@printf "should be:  0x%08x 0x%08x 0x%08x 0x%08x\n" 0x1000 0x300 0x30 0x7
	./$< tests/Instance1 | grep -e "^result"
	@printf "should be:  0x%08x 0x%08x\n" 0x55 0x11
	./$< tests/Instance2 | grep -e "^result"
	@printf "should be:  0x%08x 0x%08x\n" 0x198 0x22
	./$< tests/Instance3 | grep -e "^result"
	@printf "should be:  0x%08x 0x%08x\n" 0x33 0x44
	./$< tests/Native2 | grep -e "^result"
	@printf "should be:   0x%08x\n" 0x1337
	./$< tests/Native3 | egrep -e "^result"
	@printf "should be: %s\n" "Hello World"
	./$< tests/Strings1 | egrep -c -e "^result"
	@printf "should be: %d\n" 3
	./$< tests/Array1 | grep -e "^result"
	@printf "should be:   0x%08x 0x%08x\n" 0x264 0x8
	./$< tests/Integer1 | grep -e "^result"
	@printf "should be:  0x%08x\n" 0x1337
	./$< tests/VarArgs1 | grep -e "^result"

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
