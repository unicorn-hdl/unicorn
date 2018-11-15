#!/bin/bash

# Testing solely for 'Hello World' deliverable
ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 unic.native

touch print_test.ll

./unic.native print_test.uni > print_test.ll

/usr/local/Cellar/llvm/7.0.0/bin/llc -relocation-model=pic print_test.ll

CC -o print_test print_test.s

./print_test