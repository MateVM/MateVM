#!/bin/bash
for j in \
		 "tests/BenchObjectfield" "tests/BenchStaticfield" \
		 "tests/Fib" "tests/BenchException" "tests/BenchInterface" \
		 "tests/BenchArray" "tests/BenchInstanceOf" \
		 "tests/BenchVirtual"
do
	printf "benchmark: %s\n" "$j";
	for i in "java -client" "java -cacao" "./mate.opt" "java -jamvm";
	do
		JAVA="$i" ./tools/bench.sh "$j";
	done;
	echo "";
done
