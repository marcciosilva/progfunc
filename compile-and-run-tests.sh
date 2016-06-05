#!/bin/bash
if [ ! -f Compiler ]; then
	echo -e "Iniciando compilación..."
	make
	echo -e "Compilación finalizada.\n"
fi

echo -e "Ejecuntando tests..."

./Compiler tests/ejemplo1
./Compiler tests/ejemplo2
./Compiler tests/ejemplo3
./Compiler tests/ejemplo4
./Compiler tests/ejemplo5
./Compiler tests/ejemplo6
./Compiler tests/ejemplo7

echo -e "Ejecución de tests finalizada.\n"

echo -e "Diff output:"
diff tests/ ejemplos/
