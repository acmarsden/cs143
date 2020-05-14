#!/bin/bash

OUT_DIR=./test_lexer_out

for test_file in /afs/ir.stanford.edu/class/cs143/examples/*.cl; do
  echo "Running their lexer on $test_file"
  filename=$(basename -- "$test_file")
  extension="${filename##*.}"
  filename="${filename%.*}"
  ./theirlexer $test_file > $OUT_DIR/their_$filename.out
done

