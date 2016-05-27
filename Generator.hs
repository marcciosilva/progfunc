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
	| length bdy == 0 && (length dfs == length env) = "#include stdio.h \n" 
	++ (concat [printVariable envVar | envVar <- env]) ++ "void main() { \n }"
	| otherwise = pn

printVariable :: (Name, Type) -> String
printVariable (name, type1)
	| type1 == TyInt = "int " ++ name ++ ";\n"
	| type1 == TyBool = "bool " ++ name ++ ";\n"
	| otherwise = printArray name type1

printArray :: String -> Type -> String
printArray name (TyArray i j type1)
	| type1 == TyInt = "int _" ++ name ++ "[" ++ show (j-i+1) ++ "];\n"
	| type1 == TyBool = "bool _" ++ name ++ "[" ++ show (j-i+1) ++ "];\n"
	-- si es un array de arrays, primero imprimo el tipo, el nombre y el primer rango
	-- y despues imprimo recursivamente los subrangos que vienen
	| otherwise =  printArrayType type1 ++ " _" ++ name ++ "[" ++ show (j-i+1) ++ "]"
	++ printSubArrayRange type1 ++ ";\n"


-- funcion para obtener el tipo del array de arrays
printArrayType :: Type -> String
printArrayType (TyArray ini fin ty)
	| ty == TyInt = "int"
	| ty == TyBool = "bool"
	| otherwise = printArrayType ty

-- imprime el subrango actual y el subrango siguiente recursivamente
printSubArrayRange :: Type -> String
printSubArrayRange (TyArray ini fin ty) = "[" ++ show (fin-ini+1) ++ "]" ++ printSubArrayRange ty
printSubArrayRange (TyInt) = ""
printSubArrayRange (TyBool) = ""