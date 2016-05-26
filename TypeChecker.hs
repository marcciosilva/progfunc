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
--checkProgram (Program pn defs body) = Right [("Jorge", TyInt)]
checkProgram (Program pn defs body)  
	| length defs == 0 && length body == 0 = Right []
	| length defs == 0 = case checkBody body of
							Right env -> Right env
							Left err -> Left err
	| length body == 0 = case checkVarDef defs of
							Right env -> Right env
							Left errVarDef -> Left errVarDef
	| otherwise = case checkVarDef defs of
					Right env -> case checkBody body of
										Right env -> Right env
										Left err -> Left err
					Left errVarDef -> Left errVarDef

-- checkVarDef :: [VarDef] -> Either [Error] Env
-- checkVarDef :: [VarDef] -> [Error] -> Env -> Either [Error] Env
-- checkVarDef vs errs env = 
-- 	| length vs == 0 && length errs == 0 >> return Right vs
-- 	| length vs == 0 && length errs != 0 >> return Left errs
-- 	| length vs != 0 = case checkSingleVar head vs env of
-- 							Right env -> (checkVarDef tail vs errs env) 
-- 							Left err -> (checkVarDef tail vs err:errs env)
-- checkVarDef vs =
-- 	| length vs == 0 | return Right 

-- checkSingleVar :: VarDef -> Env -> Either Error Env
-- checkSingleVar = 

checkVarDef :: [VarDef] -> Either [Error] Env
checkVarDef vs = Right [("Pirulo", TyInt)]

checkBody :: [Stmt] -> Either [Error] Env
checkBody bs = Right[("Jorge", TyInt)]
