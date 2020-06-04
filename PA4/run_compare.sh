#!/bin/bash

filename=$1
if [ -z $example ]
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

