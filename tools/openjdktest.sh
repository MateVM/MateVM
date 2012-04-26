#!/bin/bash

class2test=$1

openjdk="java -client"
openjdk_output=`mktemp`
mate="./mate"
mate_output=`mktemp`

diff_output=`mktemp`

$openjdk $class2test | grep -e '^result:' > $openjdk_output
$mate $class2test | grep -e '^result:' > $mate_output

diff $openjdk_output $mate_output > $diff_output

openjdk_lines=`cat $openjdk_output | wc -l`
mate_lines=`cat $mate_output | wc -l`
diff_lines=`cat $diff_output | wc -l`

function quit {
	rm -rf $1 $2 $3
	exit
}

if [ $openjdk_lines = 0 ]
then
	echo -e '\033[01;31mFAIL\033[0m:    ' $class2test
	echo "no output by openjdk? abort"
	quit $openjdk_output $mate_output $diff_output
fi

if [ $mate_lines = 0 ]
then
	echo -e '\033[01;31mFAIL\033[0m:    ' $class2test
	echo "no output by mate? abort"
	quit $openjdk_output $mate_output $diff_output
fi

if [ $diff_lines = 0 ]
then
	echo -e '\033[01;32mSUCCESS\033[0m: ' $class2test
else
	echo -en '\033[01;31mFAIL\033[0m:    ' $class2test
	echo "                                diff:"
	cat $diff_output
fi


quit $openjdk_output $mate_output $diff_output
