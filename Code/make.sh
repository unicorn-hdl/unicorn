#!/bin/bash
# Fake makefile bc I don't know how to make actual makefile

ocamllex scanner.mll
ocamlyacc parser.mly
ocamlc -c ast.mli
ocamlc -c parser.mli
ocamlc -c scanner.ml
ocamlc -c parser.ml

mkdir made
mv ast.cmi made/ast.cmi
mv make.sh~ made/make.sh~
mv parser.cmi made/parser.cmi
mv parser.cmo made/parser.cmo
mv parser.ml made/parser.ml
mv parser.mli made/parser.mli
mv scanner.cmi made/scanner.cmi
mv scanner.cmo made/scanner.cmo
mv scanner.ml made/scanner.ml

rm .make.sh.swp
rm .make.sh.un~
