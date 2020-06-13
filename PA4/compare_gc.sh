#!/bin/bash

OUT_DIR=test_cgen_out

filename=$(basename -- "$1")
extension="${filename##*.}"
filename="${filename%.*}"

rm -f $filename.s
rm -f our_$filename.s
rm -f $OUT_DIR/our_$filename.out

echo "Executing $OUT_DIR/our_$filename.s on SPIM"
/afs/ir.stanford.edu/class/cs143/bin/spim $OUT_DIR/our_$filename.s &> $OUT_DIR/our_$filename.out
echo "Executing $OUT_DIR/their_$filename.s on SPIM"
/afs/ir.stanford.edu/class/cs143/bin/spim $OUT_DIR/their_$filename.s &> $OUT_DIR/their_$filename.out

echo "============== Number of times we Garbage Collected "
grep -c "Garbage" our_$filename.out
echo "============== Number of times they Garbage Collected "
grep -c "Garbage" their_$filename.out

