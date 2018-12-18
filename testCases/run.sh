#!/bin/bash

echo $1
../Code/unic.native -l $1.uni > $1.ll
/usr/local/Cellar/llvm/7.0.0/bin/llc -relocation-model=pic $1.ll
CC -o $1 $2 $1.s
./$1

echo
echo
rm $1.ll $1.s
#CC -o $1 $1.s


