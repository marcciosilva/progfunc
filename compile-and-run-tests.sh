#!/bin/bash
if [ ! -f Compiler ]; then
    make
fi

# #compile ejemplo1-ejemplo7
# for i in `seq 1 8`
# do
# 	./Compiler ejemplo$i
# done

./Compiler test1
./Compiler test1err
./Compiler test2
./Compiler test2err
./Compiler test3
./Compiler test3err
./Compiler test4
./Compiler test4err
./Compiler test5
./Compiler test5err
./Compiler test6
./Compiler test6err
./Compiler test7
./Compiler test7err
./Compiler test8
./Compiler test8err
./Compiler test9
./Compiler test9err
./Compiler test10
./Compiler test10err

# #prueba 1-3
# for i in `seq 1 3`
# do
# 	echo "Testeando ejemplo $i"
# 	diff -w out/ejemplo$i.c out-expected/ejemplo$i.c
# done

# #prueba 4-8
# for i in `seq 4 8`
# do
# 	echo "Testeando ejemplo $i"
# 	diff -w out/ejemplo$i.err out-expected/ejemplo$i.err    
# done


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
