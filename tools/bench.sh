#!/bin/bash

# $1 .. class
# vm is defined by $JAVA

if [ $# = 0 ]
then
	echo "no arguments..."
	exit
fi

# $1 .. time
# $2 .. vm
# $3 .. class
function measure {
	log=benchlog
	runs=12
	take=3
	# execute three dry runs to fill caches
	(for i in 1 2 3
	do
		($1 $2 $3) > /dev/null
	done) 2> /dev/null > /dev/null

	# average on five runs
	rm -f $log; touch $log
	(for i in `seq 1 $runs`
	do
		($1 $2 $3) 2>> $log
	done) > /dev/null

	sort $log | head -n $take | awk -F"'" '{ sum += $2; run += 1 } END { print (sum / run) }' | xargs printf "%0.2f\n"
}

javavm=$JAVA
mate="./mate"

if [ $# = 1 ]
then
	timecap="time --format='%e'" # wall time
else
	timecap="time --format='%M'" # memory (max rss)
fi

# printf "bench base...\n"
base=`measure "$timecap" "$javavm" "tests/HelloWorld"`
# printf "bench $1...\n"
target=`measure "$timecap" "$javavm" "$1"`


if [ $# = 1 ]
then
	diff=`awk "END { print $target - $base }" < /dev/null`
# printf "%-15s" "$javavm"
# printf "base: %0.2fs  " $base
# printf "target: %0.2fs  " $target
# printf "diff: %0.2fs\n" $diff
	printf "%0.2f&s" $diff
else
	# bug in time(1):
	# https://groups.google.com/forum/?fromgroups=#!topic/gnu.utils.help/u1MOsHL4bhg
	targetbug=`awk "END { print $target / (4 * 1024.0) }" < /dev/null`
	printf "%0.2f&MB" $targetbug
fi
