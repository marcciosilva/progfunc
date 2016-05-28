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
	++ (concat [printVariable envVar | envVar <- env]) ++ "void main() {" 
	++ (concat [printStatement statement | statement <- bdy])) 
	++ "\n}"

printStatement :: Stmt -> String
printStatement (Asig name exps expression) = ""
printStatement (If expression bdy1 bdy2) = ""
printStatement (For name exp1 exp2 bdy) = ""
printStatement (While expression bdy) = ""
printStatement (Write expression) = ""
printStatement (Read name) = "\nscanf(\"%d\", &_" ++ name ++");"



printVariable :: (Name, Type) -> String
printVariable (name, type1)
	| type1 == TyInt = "\nint " ++ name ++ ";"
	-- los booleanos en c se expresan como int
	-- el chequeo debe ir en la parte de body
	| type1 == TyBool = "\nint " ++ name ++ ";"
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