-- LABORATORIO DE PROGRAMACION FUNCIONAL 2016
-- MODULO DE CHEQUEO DE TIPOS

-- Se debe implementar la funcion checkProgram que
-- dado un AST que representa un programa
-- retorna una lista de mensajes de error en caso
-- de que el programa no sea correcto o, en otro caso,
-- un ambiente (Env) con las variables (y sus tipos) 
-- que se declaran en el programa.  


module TypeChecker where

import Syntax
-- se pueden agregar mas importaciones 
-- en caso de ser necesario



-- TYPE CHECKER

data Error = Duplicated      Name
           | Undefined       Name
           | NotArray        Type
           | ArrayAssignment Type
           | Expected        Type Type

instance Show Error where
 show (Duplicated      n)  = "Duplicated definition: " ++ n
 show (Undefined       n)  = "Undefined: " ++ n
 show (NotArray        ty) = "Type " ++ show ty ++ " is not an array" 
 show (ArrayAssignment ty) = "Array assignment: " ++ show ty
 show (Expected    ty ty') = "Expected: " ++ show ty ++ " Actual: " ++ show ty'

checkProgram :: Program -> Either [Error] Env
checkProgram (Program pn defs body)  
	| length defs == 0 && length body == 0 = Right []
	| length defs == 0 = case checkBody [] body of
							Right env -> Right env
							Left err -> Left err
	| length body == 0 = case checkVarDef defs [] [] of
							Right env -> Right env
							Left errVarDef -> Left errVarDef
	| otherwise = case checkVarDef defs [] [] of
					Right env -> case checkBody env body of
										Right env -> Right env
										Left err -> Left err
					-- si hay error en la declaracion no se
					-- chequea el cuerpo
					Left errVarDef -> Left errVarDef

checkVarDef :: [VarDef] -> [Error] -> Env -> Either [Error] Env
checkVarDef vs errs env
	| (length vs) /= 0 = case checkSingleVar (head vs) env of
							Right env -> checkVarDef (tail vs) errs env
							-- agrego error al final para mostrar los 
							-- errores en orden de ocurrencia
							Left err -> checkVarDef (tail vs) (errs ++ [err]) env
	| length errs /= 0 = Left errs			
	-- se agregaron todas las vars a env sin errores				
	| length errs == 0 = Right env	

checkSingleVar :: VarDef -> Env -> Either Error Env
checkSingleVar (VarDef name type1) env 
	-- Variable repetida
	| containsVariable env (name, type1) = Left (Duplicated name)
	-- agrego variable al final para declararlas 
	-- en orden de ocurrencia en el .pas
	| otherwise = Right (env ++ [(name, type1)])

-- chequea si la variable ya esta definida en env
containsVariable :: Env -> (Name,Type) -> Bool
-- si se repite el identificador, la lista queda de largo > 0
-- genero lista con variables que tienen el mismo nombre que la del parametro
-- si el largo es mayor a cero, esta duplicada
containsVariable vs (name,ty) = length ([(name_temp, type_temp)| (name_temp, type_temp) <- vs, 
	name_temp == name]) > 0

checkBody :: Env -> [Stmt] -> Either [Error] Env
checkBody env stmts = case checkStatements env [] stmts of
						Right env -> Right env
						Left errs -> Left errs




checkStatements :: Env -> [Error] -> [Stmt] -> Either [Error] Env
checkStatements env errs stmts
	| (length stmts) /= 0 = case checkStatement env (head stmts) of
							Right env -> checkStatements env errs (tail stmts)
							-- agrego error al final para mostrar los 
							-- errores en orden de ocurrencia
							Left errLst -> checkStatements env (errs ++ errLst) (tail stmts)
	| length errs /= 0 = Left errs			
	-- se agregaron todas las vars a env sin errores				
	| length errs == 0 = Right env	





checkStatement :: Env -> Stmt -> Either [Error] Env
------------------ ASIGNACION DE VARIABLES ------------------
checkStatement env (Asig name exps expression) =
	if (length exps > 0) -- posiblemente asignando un arreglo
	then 
		-- si todos los indices del acceso al arreglo son enteros
		if (checkIntegerArrayIndices exps env == TyInt)
		then
			-- trato de comparar el valor de asignacion con lo que contiene el array
			case (getTypeByName name env) of
			-- si efectivamente es un array
			Just (TyArray ini fin ty) ->
				-- obtengo ultimo tipo, por si llega a ser multidimensional el arreglo
				case (getExpressionType expression env) of
					-- si la expresion tiene un tipo asignado
					Right expType ->
						if (expType /= TyInt && expType /= TyBool)
						-- asignacion de un arreglo
						then Left [(ArrayAssignment expType)]
						else 
							-- chequeo si el tipo de variables que guarda el array
							-- se corresponde con el tipo de la expresion
							if ((getArrayType ty) == expType)
							then Right env
							else Left [(Expected (getArrayType ty) expType)]
					-- si la expresion esta indefinida por ejemplo
					Left err -> Left [err]
			Just (TyInt) -> Left [(NotArray TyInt)] -- no era un array lo de la izquierda
			Just (TyBool) -> Left [(NotArray TyBool)] -- no era un array lo de la izquierda
			Nothing -> Left [(Undefined name)] -- no pasa, dado que length exps > 0; es un array
		-- tiro el tipo de indices usados
		else Left [(Expected TyInt (checkIntegerArrayIndices exps env))]
	-- si es una unica variable
	else
		case (getExpressionType expression env) of
			Right expType ->
				if (expType /= TyInt && expType /= TyBool)
				-- asignacion de un arreglo
				then Left [(ArrayAssignment expType)]
				else
					case (getTypeByName name env) of
						Just ty ->
							if (ty == expType)
							then Right env
							else Left [(Expected ty expType)]
						Nothing -> Left [(Undefined name)]
			Left err -> Left [err]
------------------ ASIGNACION DE VARIABLES ------------------
------------------ IF ------------------
checkStatement env (If expression bdy1 bdy2) = 
	case (getExpressionType expression env) of
		Right expType ->
			-- si no es una condicion booleana
			if (expType /= TyBool)
			then Left [(Expected TyBool expType)]
			else
				-- chequeo del cuerpo del if
				case checkBody env bdy1 of
					Right env ->
						-- chequeo del cuerpo del else
						case checkBody env bdy2 of
							Right env -> Right env
							-- ya viene una lista de errores
							Left errs -> Left errs
					Left errs -> Left errs
		Left err -> Left [err]
------------------ IF ------------------
------------------ FOR ------------------
checkStatement env (For name exp1 exp2 bdy) = Right env
------------------ FOR ------------------
------------------ WHILE ------------------
checkStatement env (While expression bdy) = Right env
------------------ WHILE ------------------
------------------ WRITE ------------------
checkStatement env (Write expression) = 
	case (getExpressionType expression env) of
		Right expType ->
			if (expType /= TyInt)
			-- lo que se le pasa al writeln no es de tipo entero
			then Left [(Expected TyInt expType)]
			else Right env
		Left err -> Left [err]
------------------ WRITE ------------------
------------------ READ ------------------
checkStatement env (Read name) = 
	-- si la variable esta indefinida a la hora de usarla
	-- if (checkVarDefined name env)
	-- then Right env
	-- else Left (Undefined name)
	case (getTypeByName name env) of
		Just ty -> 
			if (ty /= TyInt)
			-- lo que se le pasa al writeln no es de tipo entero
			then Left [(Expected TyInt ty)]
			else Right env
		Nothing -> Left [(Undefined name)]
------------------ READ ------------------

getArrayType :: Type -> Type
getArrayType(TyInt) = TyInt
getArrayType(TyBool) = TyBool
getArrayType(TyArray ini fin ty) = getArrayType ty

getTypeByName :: String -> Env -> Maybe Type
getTypeByName name env = lookup name env

-- determina si una variable dada por su nombre y tipo esta
-- definida en env
checkSameType :: String -> Env -> Type -> Bool
checkSameType name env (TyArray ini fin ty) = length([var | var <- env, (fst var) == name && (snd var) == ty]) > 0
checkSameType name env (TyInt) = length([var | var <- env, (fst var) == name && (snd var) == TyInt]) > 0
checkSameType name env (TyBool) = length([var | var <- env, (fst var) == name && (snd var) == TyBool]) > 0


checkIntegerArrayIndices :: [Expr] -> Env -> Type
checkIntegerArrayIndices exps env =
	case (getExpressionType (head exps) env) of
		Right ty ->
			if (length (tail exps) > 0)
			then
				if (ty == TyInt)
				-- si el actual es entero, sigo con el resto
				then checkIntegerArrayIndices (tail exps) env
				else ty
			else ty -- si los indices son enteros devuelvo true
		Left err -> TyInt -- no sucede

-- encuentra el tipo de una expresion
getExpressionType :: Expr -> Env -> Either Error Type
-- acceso a arreglo
-- asumiendo que los indices son del mismo tipo y esta todo bien
getExpressionType (Var name exps) env
	-- si es arreglo multidimensional, obtiene el tipo recursivamente
	| length exps /= 0 = getExpressionType (head exps) env
	-- si es simplemente un nombre, lo devuelve
	| otherwise        = case (getTypeByName name env) of
							Just ty -> Right ty
							Nothing -> Left (Undefined name)
getExpressionType (IntLit int) env = Right TyInt
getExpressionType (BoolLit bool) env = Right TyBool
getExpressionType (Unary uop expr) env = getExpressionType expr env
getExpressionType (Binary bop exp1 exp2) env
	| bop == Or || bop == And || bop == Equ || bop == Less = Right TyBool
	| otherwise = Right TyInt

-- true si esta definida
checkVarDefined :: String -> Env -> Bool
checkVarDefined name env = length([name_temp | (name_temp, type_temp) <- env, name_temp == name]) > 0