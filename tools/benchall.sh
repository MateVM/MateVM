#!/bin/bash

function benchall {
	for j in \
			 "tests/HelloWorld" "tests/Fib" \
			 "tests/BenchObjectfield" "tests/BenchStaticfield" \
			 "tests/BenchVirtual" "tests/BenchInterface" \
			 "tests/BenchInstanceOf" \
			 "tests/BenchArray" \
			 "tests/BenchException" \
			 "tests/BenchCompiletime"
	do
		printf "%s &" "$j";
		for i in "java -server" "java -client" "java -cacao" "./mate.opt" "java -jamvm";
		do
			JAVA="$i" ./tools/bench.sh "$j" $1;
			printf "& ";
		done;
		printf "\\";
		printf "\\ \n";
	done
}

benchall
benchall mem
