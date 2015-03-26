#!/bin/bash
printf "Make Clean Results:\n"
make clean 
printf "\n"
printf "Make Results:\n"
make -f Makefile || exit
printf "\n"
for FILE in tests/*.rs
   do
     echo $FILE "results:"
     ./pa2 $FILE
     printf "\n"
   done
