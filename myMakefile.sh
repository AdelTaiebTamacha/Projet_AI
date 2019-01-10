#!/bin/bash

echo "Compiling ..."
ocamlc graphics.cma intersection.ml type.ml conversion.ml init.ml interface.ml main.ml -o main 
echo "Done"
echo "Create documentation ..."
cd mli && mv *.mli .. && cd .. && ocamldoc -html -d doc -t "Recherche plus court chemin" -intro Intro *.mli && mv *.mli mli
echo "Done"
rm -f *.cmi *.cmo
