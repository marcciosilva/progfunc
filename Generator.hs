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
	| length env == 0 && length bdy == 0 = "#include <stdio.h>\nvoid main() {\n}"
	| length bdy == 0 && (length dfs == length env) = "#include <stdio.h>\n" 
	++ (concat [printVariable envVar | envVar <- env]) ++ "\nvoid main() {\n}"
	-- init para sacar el ultimo ; de las sentencias
	| otherwise = "#include <stdio.h>\n" 
	++ (concat [printVariable envVar | envVar <- env]) 
	++ "void main() {\n" 
	++ (printBody bdy env)
	++ "}"

printBody :: [Stmt] -> Env -> String
printBody stmts env = concat [printStatement statement env | statement <- stmts]

printVariable :: (Name, Type) -> String
printVariable (name, type1)
	| type1 == TyInt = "int _" ++ name ++ ";\n"
	-- los booleanos en c se expresan como int
	-- el chequeo debe ir en la parte de body
	| type1 == TyBool = "int _" ++ name ++ ";\n"
	| otherwise = printArray name type1

printArray :: String -> Type -> String
printArray name (TyArray i j type1)
	| type1 == TyInt = "int _" ++ name ++ "[" ++ show (j-i+1) ++ "];\n"
	| type1 == TyBool = "int _" ++ name ++ "[" ++ show (j-i+1) ++ "];\n"
	-- si es un array de arrays, primero imprimo el tipo, el nombre y el primer rango
	-- y despues imprimo recursivamente los subrangos que vienen
	| otherwise =  printArrayType type1 ++ " _" ++ name ++ "[" ++ show (j-i+1) ++ "]"
	++ printSubArrayRange type1 ++ ";\n"


-- funcion para obtener el tipo del array de arrays
printArrayType :: Type -> String
printArrayType (TyArray ini fin ty)
	| ty == TyInt = "int"
	| ty == TyBool = "int"
	| otherwise = printArrayType ty

-- imprime el subrango actual y el subrango siguiente recursivamente
printSubArrayRange :: Type -> String
printSubArrayRange (TyArray ini fin ty) = "[" ++ show (fin-ini+1) ++ "]" ++ printSubArrayRange ty
printSubArrayRange (TyInt) = ""
printSubArrayRange (TyBool) = ""

printStatement :: Stmt -> Env -> String
printStatement (Asig name exps expression) env =
	if (length exps > 0) 
	then "_" ++ name ++ (printExpressions exps (getArrayIndices name env) env) ++  " = " ++ (printExpression expression env) ++ ";\n"	
	else "_" ++ name ++ " = " ++ (printExpression expression env) ++ ";\n"
printStatement (If expression bdy1 bdy2) env = "if (" ++ (printExpression expression env) 
	++ "){\n" ++ (printBody bdy1 env) ++ "}else{\n" ++ (printBody bdy2 env) ++ "};\n"
printStatement (For name exp1 exp2 bdy) env = "for (" ++ "_" ++ name ++ "=" ++ printExpression exp1 env ++ ";_" ++ name ++ " <= " ++ printExpression exp2 env
											++ ";" ++ "_" ++ name ++ "++ ){\n"
											++ printBody bdy env ++ "};\n"
printStatement (While expression bdy) env = "while;\n"
printStatement (Write expression) env = "printf (\"%d\\n\"," ++ printExpression expression env ++");\n"
printStatement (Read name) env = "scanf (\"%d\", &_" ++ name ++");\n"

----------------------------------------------------------------------------------------------
-- devuelve el offset que hay que agregarle a los indices de los arreglos
getArrayIndices :: String -> Env -> [Integer]
getArrayIndices varName env =
	case (getTypeByName varName env) of
		Just (TyArray ini fin ty) -> ini : getbla ty--(getArrayIndices varName env)
		Nothing -> []

getbla :: Type -> [Integer]
getbla (TyArray ini fin ty) = ini : (getbla ty)
getbla (TyInt) = []
getbla (TyBool) = []

-- devuelve tipo de una variable dado el nombre
getTypeByName :: String -> Env -> Maybe Type
getTypeByName name env = lookup name env
----------------------------------------------------------------------------------------------

printExpressions :: [Expr] -> [Integer] -> Env -> String
printExpressions expressions offsets env = 
	if (null expressions)
	then ""
	-- asumiendo que solo los arreglos tienen listas de expresiones
	else "[" ++ printExpression (head expressions) env ++ " - " ++ (show (head offsets)) ++ "]" 
			++ printExpressions (tail expressions) (tail offsets) env

printExpression :: Expr -> Env -> String
printExpression (Var name expressions) env = "_" ++ name ++ (printExpressions expressions (getArrayIndices name env) env)
printExpression (IntLit int) env = show int
printExpression (BoolLit bool) env = 
	if bool
	then "1"
	else "0"
printExpression (Unary uop expression) env = printUnaryOperator uop ++ printExpression expression env
printExpression (Binary bop exp1 exp2) env = printExpression exp1 env ++ printBinaryOperator bop ++ printExpression exp2 env

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