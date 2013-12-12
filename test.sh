#!/bin/sh

# Temporary test script, ast only

WDJC="./wdjc"

# Set time limit for all operations
ulimit -t 30

path= 'pwd'
echo $path

for file in ./tests/*.dj
do
	echo $file
	echo ------------------------
	$WDJC -s < $file
	echo
done