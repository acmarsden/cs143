#!/bin/bash

OUT_DIR=./test_lexer_out
OUT_FILE=diffs.txt

rm -f $OUT_DIR/$OUT_FILE
for test_file in /afs/ir.stanford.edu/class/cs143/examples/*.cl; do
  echo "Running diff on parser output on $test_file"
  filename=$(basename -- "$test_file")
  extension="${filename##*.}"
  filename="${filename%.*}"
  echo "==============================================================" >> $OUT_DIR/$OUT_FILE
  echo "Parser out diff (< : ours, > : theirs) on:  $test_file " >> $OUT_DIR/$OUT_FILE
  diff $OUT_DIR/our_$filename.out $OUT_DIR/their_$filename.out >> $OUT_DIR/$OUT_FILE
  echo "" >> $OUT_DIR/$OUT_FILE
  echo "" >> $OUT_DIR/$OUT_FILE
done

