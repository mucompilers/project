#!/bin/sh

if test $# -ne 1
then
echo "Usage: $0 <path_to_executable>"
exit 1
fi

for f in *.rs
do
      echo "*** Test $f ***"
      $1 < $f > "${f%.rs}.try" 2>&1
      diff -w -B "${f%.rs}.try" "${f%.rs}.out"
      #rm "${f%.rs}.try"
done
