-- LABORATORIO DE PROGRAMACION FUNCIONAL 2016
-- MODULO DE GENERACION DE CODIGO C

-- Se debe implementar la funcion genProgram que
-- dado un AST que representa un programa valido
-- y un ambiente con las variables definidas
-- genera el codigo C correspondiente


module Generator where

import Syntax
-- se pueden agregar mas importaciones 
-- en caso de ser necesario


-- CODE GENERATOR


genProgram :: Program -> Env -> String
genProgram (Program pn dfs bdy) env
	-- no variables defined
	| length env == 0 && length bdy == 0 = "#include stdio.h \n void main() { \n }"
	| length bdy == 0 && (length dfs == length env) = "#include stdio.h" 
	++ (concat [printVariable envVar | envVar <- env]) ++ "\nvoid main() { \n }"
	-- init para sacar el ultimo ; de las sentencias
	| otherwise = init ("#include stdio.h" 
	++ (concat [printVariable envVar | envVar <- env]) 
	++ "void main() {" 
	++ (printBody bdy ))
	++ "\n}"

printBody :: [Stmt] -> String
printBody stmts = concat [printStatement statement | statement <- stmts]

printVariable :: (Name, Type) -> String
printVariable (name, type1)
	| type1 == TyInt = "\nint _" ++ name ++ ";"
	-- los booleanos en c se expresan como int
	-- el chequeo debe ir en la parte de body
	| type1 == TyBool = "\nint _" ++ name ++ ";"
	| otherwise = printArray name type1

printArray :: String -> Type -> String
printArray name (TyArray i j type1)
	| type1 == TyInt = "\nint _" ++ name ++ "[" ++ show (j-i+1) ++ "];"
	| type1 == TyBool = "\nint _" ++ name ++ "[" ++ show (j-i+1) ++ "];"
	-- si es un array de arrays, primero imprimo el tipo, el nombre y el primer rango
	-- y despues imprimo recursivamente los subrangos que vienen
	| otherwise =  printArrayType type1 ++ " _" ++ name ++ "[" ++ show (j-i+1) ++ "]"
	++ printSubArrayRange type1 ++ ";\n"


-- funcion para obtener el tipo del array de arrays
printArrayType :: Type -> String
printArrayType (TyArray ini fin ty)
	| ty == TyInt = "\nint"
	| ty == TyBool = "\nint"
	| otherwise = printArrayType ty

-- imprime el subrango actual y el subrango siguiente recursivamente
printSubArrayRange :: Type -> String
printSubArrayRange (TyArray ini fin ty) = "[" ++ show (fin-ini+1) ++ "]" ++ printSubArrayRange ty
printSubArrayRange (TyInt) = ""
printSubArrayRange (TyBool) = ""

printStatement :: Stmt -> String
printStatement (Asig name exps expression) =
	if (length exps > 0) 
	then "\n_" ++ name ++ (printExpressions exps) ++  " = " ++ (printExpression expression) ++ ";"	
	else "\n_" ++ name ++ " = " ++ (printExpression expression) ++ ";"
printStatement (If expression bdy1 bdy2) = "\nif" ++ (printExpression expression) 
	++ " {\n" ++ (printBody bdy1) ++ "\n}{" ++ (printBody bdy2) ++ "\n}"
printStatement (For name exp1 exp2 bdy) = ""
printStatement (While expression bdy) = ""
printStatement (Write expression) = ""
printStatement (Read name) = "\nscanf(\"%d\", &_" ++ name ++");"

printExpressions :: [Expr] -> String
printExpressions expressions = 
	if (length expressions) == 0
	then ""
	-- asumiendo que solo los arreglos tienen listas de expresiones
	else "[" ++ printExpression (head expressions) ++ "]" ++ printExpressions (tail expressions)

printExpression :: Expr -> String
printExpression (Var name expressions) = "_" ++ name ++ (printExpressions expressions)
printExpression (IntLit int) = show int
printExpression (BoolLit bool) = 
	if bool
	then "1"
	else "0"
printExpression (Unary uop expression) = printUnaryOperator uop ++ printExpression expression
printExpression (Binary bop exp1 exp2) = printExpression exp1 ++ printBinaryOperator bop 
										++ printExpression exp2

printUnaryOperator :: UOp -> String
printUnaryOperator (Not) = "!"
printUnaryOperator (Neg) = "-"

printBinaryOperator :: BOp -> String
printBinaryOperator (Or) = " || "
printBinaryOperator (And) = " && "
printBinaryOperator (Equ) = " == "
printBinaryOperator (Less) = " < "
printBinaryOperator (Plus) = " + "
printBinaryOperator (Minus) = " - "
printBinaryOperator (Mult) = " * "
printBinaryOperator (Div) = " / "
printBinaryOperator (Mod) = " % "










