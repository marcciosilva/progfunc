Compiler : Compiler.hs Syntax.hs TypeChecker.hs Generator.hs
	ghc --make Compiler

clean :
	rm -f *.hi *.o Compiler
	rm -f out/*.c out/*.err
