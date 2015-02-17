#!/bin/sh

if test $# -ne 1
then
echo "Usage: $0 <path_to_executable>"
exit 1
fi

for f in *.rs
do
      echo "*** Test $f ***"
      $1 < $f > out.tmp 2>&1
      diff -w -B out.tmp "${f%.rs}.out"
      rm out.tmp
done
