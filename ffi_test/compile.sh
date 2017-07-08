#!/bin/bash
ocamlfind ocamlopt -linkpkg -package ctypes,ctypes.foreign -cclib -Wl,-E dummy.o test.ml -o test.exe
