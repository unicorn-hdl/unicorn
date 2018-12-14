#!/bin/bash

# Testing solely for 'Hello World' deliverable
ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 unic.native

touch print_test.ll

./unic.native tests/new.uni > ./tests/print_test.ll

/usr/local/Cellar/llvm/7.0.0/bin/llc -relocation-model=pic ./tests/print_test.ll

CC -o ./tests/print_test ./tests/print_test.s

./print_test
