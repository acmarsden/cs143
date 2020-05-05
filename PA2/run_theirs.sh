#!/bin/bash

OUT_DIR=./test_parser_out

for test_file in /afs/ir.stanford.edu/class/cs143/examples/*.cl; do
  echo "Running their parser on $test_file"
  filename=$(basename -- "$test_file")
  extension="${filename##*.}"
  filename="${filename%.*}"
  ./theirparser $test_file > $OUT_DIR/their_$filename.out
done

