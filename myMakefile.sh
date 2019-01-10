#!/bin/bash

echo "Compiling ..."
ocamlc graphics.cma intersection.ml type.ml conversion.ml init.ml interface.ml main.ml -o main && rm -f *.cmi *.cmo
echo "Done"
