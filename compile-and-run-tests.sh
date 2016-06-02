#!/bin/bash
if [ ! -f Compiler ]; then
    make
fi

./Compiler tests/ejemplo1
./Compiler tests/ejemplo2
./Compiler tests/ejemplo3
./Compiler tests/ejemplo4
./Compiler tests/ejemplo5
./Compiler tests/ejemplo6
./Compiler tests/ejemplo7

diff tests/ ejemplos/
