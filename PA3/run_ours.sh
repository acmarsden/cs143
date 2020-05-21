#!/bin/bash

OUT_DIR=./test_semant_out

for test_file in /afs/ir.stanford.edu/class/cs143/examples/*.cl; do
  echo "Running our semant on $test_file"
  filename=$(basename -- "$test_file")
  extension="${filename##*.}"
  filename="${filename%.*}"
  ./mysemant $test_file > $OUT_DIR/our_$filename.out
done

