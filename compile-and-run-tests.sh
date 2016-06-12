#!/bin/bash
if [ ! -f Compiler ]; then
    make
fi

for i in {1..8}
do
	./Compiler tests/ejemplo${i}
done

for i in {1..10}
do
	./Compiler tests/test${i}
	./Compiler tests/test${i}err
done

echo "Testeando ejemplo1"
diff -w out/ejemplo1.c out-expected/ejemplo1.c
echo "Testeando ejemplo2"
diff -w out/ejemplo2.c out-expected/ejemplo2.c
echo "Testeando ejemplo3"
diff -w out/ejemplo3.c out-expected/ejemplo3.c
echo "Testeando ejemplo4"
diff -w out/ejemplo4.err out-expected/ejemplo4.err
echo "Testeando ejemplo5"
diff -w out/ejemplo5.err out-expected/ejemplo5.err
echo "Testeando ejemplo6"
diff -w out/ejemplo6.err out-expected/ejemplo6.err
echo "Testeando ejemplo7"
diff -w out/ejemplo7.err out-expected/ejemplo7.err
echo "Testeando ejemplo8"
diff -w out/ejemplo8.err out-expected/ejemplo8.err
echo "Testeando test1"
diff -w out/test1.c out-expected/test1.c
echo "Testeando test1err"
diff -w out/test1err.err out-expected/test1err.err
echo "Testeando test2"
diff -w out/test2.c out-expected/test2.c
echo "Testeando test2err"
diff -w out/test2err.err out-expected/test2err.err
echo "Testeando test3"
diff -w out/test3.c out-expected/test3.c
echo "Testeando test3err"
diff -w out/test3err.err out-expected/test3err.err
echo "Testeando test4"
diff -w out/test4.c out-expected/test4.c
echo "Testeando test4err"
diff -w out/test4err.err out-expected/test4err.err
echo "Testeando test5"
diff -w out/test5.c out-expected/test5.c
echo "Testeando test5err"
diff -w out/test5err.err out-expected/test5err.err
echo "Testeando test6"
diff -w out/test6.c out-expected/test6.c
echo "Testeando test6err"
diff -w out/test6err.err out-expected/test6err.err
echo "Testeando test7"
diff -w out/test7.c out-expected/test7.c
echo "Testeando test7err"
diff -w out/test7err.err out-expected/test7err.err
echo "Testeando test8"
diff -w out/test8.c out-expected/test8.c
echo "Testeando test8err"
diff -w out/test8err.err out-expected/test8err.err
echo "Testeando test9"
diff -w out/test9.c out-expected/test9.c
echo "Testeando test9err"
diff -w out/test9err.err out-expected/test9err.err
echo "Testeando test10"
diff -w out/test10.c out-expected/test10.c
echo "Testeando test10err"
diff -w out/test10err.err out-expected/test10err.err
