#!/bin/bash

OUT_DIR=$2

filename=$(basename -- "$1")
extension="${filename##*.}"
filename="${filename%.*}"

rm -f $filename.s
rm -f their_$filename.s
rm -f $OUT_DIR/their_$filename.out

echo "Running our compiler on $filename.cl"
./theircoolc $1

echo "Saving to $OUT_DIR/their_$filename.s"
mv $filename.s $OUT_DIR/their_$filename.s

echo "Executing $OUT_DIR/our_$filename.s on SPIM"
/afs/ir.stanford.edu/class/cs143/bin/spim $OUT_DIR/their_$filename.s &> $OUT_DIR/their_$filename.out
