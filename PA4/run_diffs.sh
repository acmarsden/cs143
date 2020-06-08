#!/bin/bash

OUT_DIR=./test_cgen_out

for test_file in /afs/ir.stanford.edu/class/cs143/examples/*.cl; do
  echo "==================== Running cgen on $test_file"
  filename=$(basename -- "$test_file")
  extension="${filename##*.}"
  filename="${filename%.*}"
  ./run_ours.sh $test_file $OUT_DIR
  ./run_theirs.sh $test_file $OUT_DIR
done

for test_file in *.cl; do
  echo "==================== Running cgen on $test_file"
  filename=$(basename -- "$test_file")
  extension="${filename##*.}"
  filename="${filename%.*}"
  ./run_ours.sh $test_file $OUT_DIR
  ./run_theirs.sh $test_file $OUT_DIR
done

echo "Computing diffs in output"

OUT_FILE=diffs.txt

rm -f $OUT_DIR/$OUT_FILE
for test_file in /afs/ir.stanford.edu/class/cs143/examples/*.cl; do
  echo "Running diff on SPIM output for $test_file"
  filename=$(basename -- "$test_file")
  extension="${filename##*.}"
  filename="${filename%.*}"
  echo "==============================================================" >> $OUT_DIR/$OUT_FILE
  echo "SPIM out diff (< : ours, > : theirs) on:  $test_file " >> $OUT_DIR/$OUT_FILE
  diff $OUT_DIR/our_$filename.out $OUT_DIR/their_$filename.out >> $OUT_DIR/$OUT_FILE
  echo "" >> $OUT_DIR/$OUT_FILE
  echo "" >> $OUT_DIR/$OUT_FILE
done

for test_file in *.cl; do
  echo "Running diff on SPIM output for $test_file"
  filename=$(basename -- "$test_file")
  extension="${filename##*.}"
  filename="${filename%.*}"
  echo "==============================================================" >> $OUT_DIR/$OUT_FILE
  echo "SPIM out diff (< : ours, > : theirs) on:  $test_file " >> $OUT_DIR/$OUT_FILE
  diff $OUT_DIR/our_$filename.out $OUT_DIR/their_$filename.out >> $OUT_DIR/$OUT_FILE
  echo "" >> $OUT_DIR/$OUT_FILE
  echo "" >> $OUT_DIR/$OUT_FILE
done
