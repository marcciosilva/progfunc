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
							Left err -> checkStatements env (errs ++ [err]) (tail stmts)
	| length errs /= 0 = Left errs			
	-- se agregaron todas las vars a env sin errores				
	| length errs == 0 = Right env	





checkStatement :: Env -> Stmt -> Either Error Env
checkStatement env (Asig name exps expression) =
	if (length exps > 0)
	then if (checkVarDefined name env)
		then if (checkSameType name env (getExpressionType expression))
			then Right env
			else 
				case (potentialType) of
					Just (TyArray ini fin ty) -> 
						-- obtengo el tipo recursivamente por si 
						-- es arreglo multidimensional
						if (getArrayType ty) == (getExpressionType expression)
						then Right env
						else 
							Left (Expected (getArrayType ty) (getExpressionType expression))
					Nothing -> Right env -- nunca pasaria en realidad
				-- Left (Expected (getArrayType name) TyBool)
		else Left (Undefined name)
		-- if inArrayRange exps
	else Right env
	-- de alguna manera tengo que conseguir el tipo inicial
	where potentialType = getTypeByName name env

checkStatement env (If expression bdy1 bdy2) = Right env
checkStatement env (For name exp1 exp2 bdy) = Right env
checkStatement env (While expression bdy) = Right env
checkStatement env (Write expression) = Right env
checkStatement env (Read name) = 
	-- si la variable esta indefinida a la hora de usarla
	if (checkVarDefined name env)
	then Right env
	else Left (Undefined name)


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

-- encuentra el tipo de una expresion
getExpressionType :: Expr -> Type
-- acceso a arreglo
-- asumiendo que los indices son del mismo tipo y esta todo bien
-- dummy indices
getExpressionType(Var name exps) = TyArray 0 0 (getExpressionType (head exps))
getExpressionType(IntLit int) = TyInt
getExpressionType(BoolLit bool) = TyBool
getExpressionType(Unary uop expr) = getExpressionType expr
getExpressionType(Binary bop exp1 exp2) 
	| bop == Or || bop == And || bop == Equ || bop == Less = TyBool
	| otherwise = TyInt

-- true si esta definida
checkVarDefined :: String -> Env -> Bool
checkVarDefined name env = length([name_temp | (name_temp, type_temp) <- env, name_temp == name]) > 0