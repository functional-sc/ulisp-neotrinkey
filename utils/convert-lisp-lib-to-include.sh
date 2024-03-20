#!/bin/bash

# strips full-line comments from .lisp file and adds headers/footer
# for inclusion into a C-style program

if [ -z "$1" ]; then
    echo "ERROR expected .lisp file to conver to .h"
    exit
fi

echo "/*"
echo " * generated from LispLibrary.lisp using convert-lisp-lib-to-include.sh"
echo " */"
echo ""
echo "const char LispLibrary[] PROGMEM = R\"lisplibrary("
echo ""
echo ""
echo ""
echo ""

grep -v ";;" "$1"

echo ""
echo ""
echo ""
echo ""
echo ")lisplibrary\";"


