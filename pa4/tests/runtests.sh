#!/bin/sh

if test $# -ne 1
then
echo "Usage: $0 <path_to_executable>"
exit 1
fi

for f in *.rs
do
      echo "*** Test $f ***"
      $1 < $f > "${f%.rs}.ll" 2>&1
      clang "${f%.rs}.ll" 2>&1
      ./a.out > "${f%.rs}.try" 2>&1
      diff "${f%.rs}.try" "${f%.rs}.out"
      rm a.out "${f%.rs}.try"
done
