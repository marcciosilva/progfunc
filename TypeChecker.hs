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
	| length defs == 0 = case checkBody body of
							Right env -> Right env
							Left err -> Left err
	| length body == 0 = case checkVarDef defs [] [] of
							Right env -> Right env
							Left errVarDef -> Left errVarDef
	| otherwise = case checkVarDef defs [] [] of
					Right env -> case checkBody body of
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


checkBody :: [Stmt] -> Either [Error] Env
checkBody bs = Right[("Jorge", TyInt)]

containsVariable :: Env -> (Name,Type) -> Bool
-- si se repite el identificador, la lista queda de largo > 0
-- genero lista con variables que tienen el mismo nombre que la del parametro
-- si el largo es mayor a cero, esta duplicada
containsVariable vs (name,ty) = length ([(name_temp, type_temp)| (name_temp, type_temp) <- vs, 
	name_temp == name]) > 0