#!/bin/bash

class2test=$1

openjdk="java -client"
openjdk_output=`mktemp`
mate=./mate
mate_output=`mktemp`

diff_output=`mktemp`

$openjdk $class2test | grep -e '^result:' > $openjdk_output
$mate $class2test | grep -e '^result:' > $mate_output

diff $openjdk_output $mate_output > $diff_output

diff_lines=`cat $diff_output | wc -l`


if [ $diff_lines = 0 ]
then
	echo -e '\033[01;32mSUCCESS\033[0m: ' $class2test
else
	echo -en '\033[01;31mFAIL\033[0m:    ' $class2test
	echo "                                diff:"
	cat $diff_output
fi


rm -f $openjdk_output $mate_output $diff_output
