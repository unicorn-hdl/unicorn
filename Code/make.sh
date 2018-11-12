#!/bin/bash
# Fake makefile bc I don't know how to make actual makefile

ocamllex scanner.mll
	echo "scanner.mll compiled"
ocamlyacc -v parser.mly
	echo "parser.mly compiled"
ocamlc -c ast.ml
	echo "ast.ml compiled"
ocamlc -c parser.mli
	echo "parser.mli compiled"
ocamlc -c scanner.ml
	echo "scanner.ml compiled"
ocamlc -c parser.ml
	echo "parser.ml compiled"
ocamlc -c printer.ml
	echo "printer.ml compiled"
ocamlc -o printer parser.cmo scanner.cmo printer.cmo

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
