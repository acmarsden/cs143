#!/bin/bash

OUT_DIR=./test_parser_out

for test_file in /afs/ir.stanford.edu/class/cs143/examples/*.cl; do
  echo "Running our parser on $test_file"
  filename=$(basename -- "$test_file")
  extension="${filename##*.}"
  filename="${filename%.*}"
  ./myparser $test_file > $OUT_DIR/our_$filename.out
done

