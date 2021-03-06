#!/bin/bash

filename=$1
if [ -z $filename ]
then
    filename="example"
fi

rm -f our_$filename.s
rm -f their_$filename.s

echo "Running their compiler on $filename.cl"
./theircoolc $filename.cl

echo "Saving to their_$filename.s"
mv $filename.s their_$filename.s

echo "Running our compiler on $filename.cl"
./mycoolc $filename.cl

echo "Saving to our_$filename.s"
mv $filename.s our_$filename.s

class_bin=/usr/class/cs143/bin
echo "=============== Running theirs ==================="
$class_bin/spim their_$filename.s

echo "================Running ours ================="
$class_bin/spim our_$filename.s

