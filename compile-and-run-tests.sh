#!/bin/bash
if [ ! -f Compiler ]; then
    make
fi

#toma entradas del inco
./Compiler ejemplo1
./Compiler ejemplo2
./Compiler ejemplo3
./Compiler ejemplo4
./Compiler ejemplo5
./Compiler ejemplo6
./Compiler ejemplo7

#test 1-3
for i in `seq 1 3`
do
	echo "Testeando ejemplo $i"
	diff out/ejemplo$i.c out-expected/ejemplo$i.c
done

#test 4-8
for i in `seq 4 8`
do
	echo "Testeando ejemplo $i"
	diff out/ejemplo$i.err out-expected/ejemplo$i.err    
done

#diff tests/ejemplo8.err ejemplos/ejemplo8.err    