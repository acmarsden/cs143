#!/bin/bash

OUT_DIR=./test_semant_out

for test_file in /afs/ir.stanford.edu/class/cs143/examples/*.cl; do
  echo "Running their semant on $test_file"
  filename=$(basename -- "$test_file")
  extension="${filename##*.}"
  filename="${filename%.*}"
  ./theirsemant $test_file &> $OUT_DIR/their_$filename.out
done

