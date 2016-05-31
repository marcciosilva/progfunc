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


-- recibe una lista vacia de errores
checkForStatement :: Env -> [Error] -> Expr -> Expr -> Body -> Either [Error] Env
checkForStatement env errs exp1 exp2 bdy =
	-- chequeo si variable de iteracion default esta definida
	if ((checkVarDefined "i" env) == False)
	then
		-- le paso una lista vacia para errores, porque errs igual la concateno despues
		case (getExpressionType [] exp1 env) of
			Right exp1Type ->
				if (exp1Type /= TyInt)
				then
					-- aca al menos exp1 salió mal, hay que ver que pasa con exp2 y bdy
					case (getExpressionType [] exp2 env) of
						Right exp2Type ->
							if (exp2Type /= TyInt)
							then 
								-- aca falla por lo menos la exp2, y el bdy puede fallar o no
								case checkBody env bdy of
									Right env -> Left (errs ++ [(Undefined "i")] ++ [(Expected TyInt exp1Type)] ++ [(Expected TyInt exp2Type)])
									-- todo tiene errores
									Left errLst -> Left (errs ++ [(Undefined "i")] ++ [(Expected TyInt exp1Type)] ++ [(Expected TyInt exp2Type)] ++ errLst)
							else 
								-- falla exp1, no falla exp2, hay que ver el bdy
								case checkBody env bdy of
									Right env -> Left (errs ++ [(Undefined "i")] ++ [(Expected TyInt exp1Type)])
									-- fallan exp1 mas el bdy
									Left errLst -> Left (errs ++ [(Undefined "i")] ++ [(Expected TyInt exp1Type)] ++ errLst)
						-- si alguna variable no esta definida en exp2
						Left errs1 ->
							-- ya se sabe que falló exp2, y que el exptype de exp1 no matchea
							case checkBody env bdy of
								-- el body esta bien pero lo demas tiene errores
								Right env -> Left (errs ++ [(Undefined "i")] ++ [(Expected TyInt exp1Type)] ++ errs1)
								-- todo tiene errores
								Left errLst -> Left (errs ++ [(Undefined "i")] ++ [(Expected TyInt exp1Type)] ++ errs1 ++ errLst)
				else
					-- aca al menos exp 1 salio bien
					case (getExpressionType [] exp2 env) of
						Right exp2Type ->
							if (exp2Type /= TyInt)
							then 
								-- aca falla por lo menos la exp2, y el bdy puede fallar o no
								case checkBody env bdy of
									Right env -> Left (errs ++ [(Undefined "i")] ++ [(Expected TyInt exp2Type)])
									-- ya viene una lista de errores
									Left errLst -> Left (errs ++ [(Undefined "i")] ++ [(Expected TyInt exp2Type)] ++ errLst)
							else 
								-- en esta bifurcacion sale todo bien, o solo falla el body
								case checkBody env bdy of
									-- sale todo bien pero como falla la definicion de i, marchamos
									Right env -> Left (errs ++ [(Undefined "i")])
									-- ya viene una lista de errores
									Left errLst -> Left (errs ++ [(Undefined "i")] ++ errLst)
						Left errs1 ->
							-- exp2 esta mal, y exp 1 bien
							case checkBody env bdy of
								Right env -> Left (errs ++ [(Undefined "i")] ++ errs1)
								-- ya viene una lista de errores
								Left errLst -> Left (errs ++ [(Undefined "i")] ++ errs1 ++ errLst)
			Left errs1 -> --exp1 salio mal de unaLeft errs ++ [err]	
				-- aca al menos exp1 salió mal, hay que ver que pasa con exp2 y bdy
				case (getExpressionType [] exp2 env) of
					Right exp2Type ->
						if (exp2Type /= TyInt)
						then 
							-- aca falla por lo menos la exp2, y el bdy puede fallar o no
							case checkBody env bdy of
								Right env -> Left (errs ++ [(Undefined "i")] ++ errs1 ++ [(Expected TyInt exp2Type)])
								-- todo tiene errores
								Left errLst -> Left (errs ++ [(Undefined "i")] ++ errs1 ++ [(Expected TyInt exp2Type)] ++ errLst)
						else 
							-- falla exp1, no falla exp2, hay que ver el bdy
							case checkBody env bdy of
								Right env -> Left (errs ++ [(Undefined "i")] ++ errs1)
								-- fallan exp1 mas el bdy
								Left errLst -> Left (errs ++ [(Undefined "i")] ++ errs1 ++ errLst)
					-- si alguna variable no esta definida en exp2
					Left errs2 ->
						-- ya se sabe que falló exp2, y que el exptype de exp1 no matchea
						case checkBody env bdy of
							-- el body esta bien pero lo demas tiene errores
							Right env -> Left (errs ++ [(Undefined "i")] ++ errs1 ++ errs2)
							-- todo tiene errores
							Left errLst -> Left (errs ++ [(Undefined "i")] ++ errs1 ++ errs2 ++ errLst)
	else
		-- chequeo que la variable i sea de tipo entero
		case (getTypeByName "i" env) of
			Just ty ->
				if (ty == TyInt)
				then
					-- esta bien definida i como entero
					case (getExpressionType [] exp1 env) of
						Right exp1Type ->
							if (exp1Type /= TyInt)
							then
								-- aca al menos exp1 salió mal, hay que ver que pasa con exp2 y bdy
								case (getExpressionType [] exp2 env) of
									Right exp2Type ->
										if (exp2Type /= TyInt)
										then 
											-- aca falla por lo menos la exp2, y el bdy puede fallar o no
											case checkBody env bdy of
												Right env -> Left (errs ++ [(Expected TyInt exp1Type)] ++ [(Expected TyInt exp2Type)])
												-- todo tiene errores
												Left errLst -> Left (errs ++ [(Expected TyInt exp1Type)] ++ [(Expected TyInt exp2Type)] ++ errLst)
										else 
											-- falla exp1, no falla exp2, hay que ver el bdy
											case checkBody env bdy of
												Right env -> Left (errs ++ [(Expected TyInt exp1Type)])
												-- fallan exp1 mas el bdy
												Left errLst -> Left (errs ++ [(Expected TyInt exp1Type)] ++ errLst)
									-- si alguna variable no esta definida en exp2
									Left errs1 ->
										-- ya se sabe que falló exp2, y que el exptype de exp1 no matchea
										case checkBody env bdy of
											-- el body esta bien pero lo demas tiene errores
											Right env -> Left (errs ++ [(Expected TyInt exp1Type)] ++ errs1)
											-- todo tiene errores
											Left errLst -> Left (errs ++ [(Expected TyInt exp1Type)] ++ errs1 ++ errLst)
							else
								-- aca al menos exp 1 salio bien
								case (getExpressionType [] exp2 env) of
									Right exp2Type ->
										if (exp2Type /= TyInt)
										then 
											-- aca falla por lo menos la exp2, y el bdy puede fallar o no
											case checkBody env bdy of
												Right env -> Left (errs ++ [(Expected TyInt exp2Type)])
												-- ya viene una lista de errores
												Left errLst -> Left (errs ++ [(Expected TyInt exp2Type)] ++ errLst)
										else 
											-- en esta bifurcacion sale todo bien, o solo falla el body
											case checkBody env bdy of
												-- sale todo bien
												Right env -> Right env
												-- ya viene una lista de errores
												Left errLst -> Left (errs ++ errLst)
									Left errs1 ->
										-- exp2 esta mal, y exp 1 bien
										case checkBody env bdy of
											Right env -> Left (errs ++ errs1)
											-- ya viene una lista de errores
											Left errLst -> Left (errs ++ errs1 ++ errLst)
						Left errs1 -> --exp1 salio mal de unaLeft errs ++ errs1	
							-- aca al menos exp1 salió mal, hay que ver que pasa con exp2 y bdy
							case (getExpressionType [] exp2 env) of
								Right exp2Type ->
									if (exp2Type /= TyInt)
									then 
										-- aca falla por lo menos la exp2, y el bdy puede fallar o no
										case checkBody env bdy of
											Right env -> Left (errs ++ errs1 ++ [(Expected TyInt exp2Type)])
											-- todo tiene errores
											Left errLst -> Left (errs ++ errs1 ++ [(Expected TyInt exp2Type)] ++ errLst)
									else 
										-- falla exp1, no falla exp2, hay que ver el bdy
										case checkBody env bdy of
											Right env -> Left (errs ++ errs1)
											-- fallan exp1 mas el bdy
											Left errLst -> Left (errs ++ errs1 ++ errLst)
								-- si alguna variable no esta definida en exp2
								Left errs2 ->
									-- ya se sabe que falló exp2, y que el exptype de exp1 no matchea
									case checkBody env bdy of
										-- el body esta bien pero lo demas tiene errores
										Right env -> Left (errs ++ errs1 ++ errs2)
										-- todo tiene errores
										Left errLst -> Left (errs ++ errs1 ++ errs2 ++ errLst)
				else
					-- le paso una lista vacia para errores, porque errs igual la concateno despues
					case (getExpressionType [] exp1 env) of
						Right exp1Type ->
							if (exp1Type /= TyInt)
							then
								-- aca al menos exp1 salió mal, hay que ver que pasa con exp2 y bdy
								case (getExpressionType [] exp2 env) of
									Right exp2Type ->
										if (exp2Type /= TyInt)
										then 
											-- aca falla por lo menos la exp2, y el bdy puede fallar o no
											case checkBody env bdy of
												Right env -> Left (errs ++ [(Expected TyInt ty)] ++ [(Expected TyInt exp1Type)] ++ [(Expected TyInt exp2Type)])
												-- todo tiene errores
												Left errLst -> Left (errs ++ [(Expected TyInt ty)] ++ [(Expected TyInt exp1Type)] ++ [(Expected TyInt exp2Type)] ++ errLst)
										else 
											-- falla exp1, no falla exp2, hay que ver el bdy
											case checkBody env bdy of
												Right env -> Left (errs ++ [(Expected TyInt ty)] ++ [(Expected TyInt exp1Type)])
												-- fallan exp1 mas el bdy
												Left errLst -> Left (errs ++ [(Expected TyInt ty)] ++ [(Expected TyInt exp1Type)] ++ errLst)
									-- si alguna variable no esta definida en exp2
									Left errs1 ->
										-- ya se sabe que falló exp2, y que el exptype de exp1 no matchea
										case checkBody env bdy of
											-- el body esta bien pero lo demas tiene errores
											Right env -> Left (errs ++ [(Expected TyInt ty)] ++ [(Expected TyInt exp1Type)] ++ errs1)
											-- todo tiene errores
											Left errLst -> Left (errs ++ [(Expected TyInt ty)] ++ [(Expected TyInt exp1Type)] ++ errs1 ++ errLst)
							else
								-- aca al menos exp 1 salio bien
								case (getExpressionType [] exp2 env) of
									Right exp2Type ->
										if (exp2Type /= TyInt)
										then 
											-- aca falla por lo menos la exp2, y el bdy puede fallar o no
											case checkBody env bdy of
												Right env -> Left (errs ++ [(Expected TyInt ty)] ++ [(Expected TyInt exp2Type)])
												-- ya viene una lista de errores
												Left errLst -> Left (errs ++ [(Expected TyInt ty)] ++ [(Expected TyInt exp2Type)] ++ errLst)
										else 
											-- en esta bifurcacion sale todo bien, o solo falla el body
											case checkBody env bdy of
												-- sale todo bien pero como falla la definicion de i, marchamos
												Right env -> Left (errs ++ [(Expected TyInt ty)])
												-- ya viene una lista de errores
												Left errLst -> Left (errs ++ [(Expected TyInt ty)] ++ errLst)
									Left errs1 ->
										-- exp2 esta mal, y exp 1 bien
										case checkBody env bdy of
											Right env -> Left (errs ++ [(Expected TyInt ty)] ++ errs1)
											-- ya viene una lista de errores
											Left errLst -> Left (errs ++ [(Expected TyInt ty)] ++ errs1 ++ errLst)
						Left errs1 -> --exp1 salio mal de unaLeft errs ++ [err]	
							-- aca al menos exp1 salió mal, hay que ver que pasa con exp2 y bdy
							case (getExpressionType [] exp2 env) of
								Right exp2Type ->
									if (exp2Type /= TyInt)
									then 
										-- aca falla por lo menos la exp2, y el bdy puede fallar o no
										case checkBody env bdy of
											Right env -> Left (errs ++ [(Expected TyInt ty)] ++ errs1 ++ [(Expected TyInt exp2Type)])
											-- todo tiene errores
											Left errLst -> Left (errs ++ [(Expected TyInt ty)] ++ errs1 ++ [(Expected TyInt exp2Type)] ++ errLst)
									else 
										-- falla exp1, no falla exp2, hay que ver el bdy
										case checkBody env bdy of
											Right env -> Left (errs ++ [(Expected TyInt ty)] ++ errs1)
											-- fallan exp1 mas el bdy
											Left errLst -> Left (errs ++ [(Expected TyInt ty)] ++ errs1 ++ errLst)
								-- si alguna variable no esta definida en exp2
								Left errs2 ->
									-- ya se sabe que falló exp2, y que el exptype de exp1 no matchea
									case checkBody env bdy of
										-- el body esta bien pero lo demas tiene errores
										Right env -> Left (errs ++ [(Expected TyInt ty)] ++ errs1 ++ errs2)
										-- todo tiene errores
										Left errLst -> Left (errs ++ [(Expected TyInt ty)] ++ errs1 ++ errs2 ++ errLst)

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
				case (getExpressionType [] expression env) of
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
					Left errs -> Left errs
			Just (TyInt) -> Left [(NotArray TyInt)] -- no era un array lo de la izquierda
			Just (TyBool) -> Left [(NotArray TyBool)] -- no era un array lo de la izquierda
			Nothing -> Left [(Undefined name)] -- no pasa, dado que length exps > 0; es un array
		-- tiro el tipo de indices usados
		else Left [(Expected TyInt (checkIntegerArrayIndices exps env))]
	-- si es una unica variable
	else
		case (getExpressionType [] expression env) of
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
			Left errs ->
					case (getTypeByName name env) of
						Just ty -> Left errs
						Nothing -> Left ([(Undefined name)] ++ errs)
------------------ ASIGNACION DE VARIABLES ------------------
------------------ IF ------------------
checkStatement env (If expression bdy1 bdy2) = 
	case (getExpressionType [] expression env) of
		Right expType ->
			-- si no es una condicion booleana
			if (expType /= TyBool)
			then 
				-- la expresion es incorrecta, chequeo el resto
				case checkBody env bdy1 of
					Right env ->
						-- chequeo del cuerpo del else
						case checkBody env bdy2 of
							-- solo la expresion falla
							Right env -> Left [(Expected TyBool expType)]
							-- ya viene una lista de errores
							Left errsBdy2 -> Left ([(Expected TyBool expType)] ++ errsBdy2)
					Left errsBdy1 ->
						-- salio mal el bdy 1, tengo que chequear el 2 igual
						case checkBody env bdy2 of
							Right env -> Left ([(Expected TyBool expType)] ++ errsBdy1)
							-- sale todo mal
							Left errsBdy2 -> Left ([(Expected TyBool expType)] ++ errsBdy1 ++ errsBdy2)
			else
				-- la expresion es correcta, chequeo el resto
				case checkBody env bdy1 of
					Right env ->
						-- chequeo del cuerpo del else
						case checkBody env bdy2 of
							-- salio todo bien
							Right env -> Right env
							-- ya viene una lista de errores
							Left errsBdy2 -> Left errsBdy2
					Left errsBdy1 ->
						-- salio mal el bdy 1, tengo que chequear el 2 igual
						case checkBody env bdy2 of
							Right env -> Left errsBdy1
							-- ya viene una lista de errores
							Left errsBdy2 -> Left (errsBdy1 ++ errsBdy2)
		Left errsExp -> 
			-- la expresion es incorrecta, chequeo el resto
			case checkBody env bdy1 of
				Right env ->
					-- chequeo del cuerpo del else
					case checkBody env bdy2 of
						Right env -> Left errsExp
						-- ya viene una lista de errores
						Left errsBdy2 -> Left (errsExp ++ errsBdy2)
				Left errsBdy1 ->
					-- salio mal el bdy 1, tengo que chequear el 2 igual
					case checkBody env bdy2 of
						Right env -> Left (errsExp ++ errsBdy1)
						-- sale todo mal
						Left errsBdy2 -> Left (errsExp ++ errsBdy1 ++ errsBdy2)
------------------ IF ------------------
------------------ FOR ------------------
checkStatement env (For name exp1 exp2 bdy) = checkForStatement env [] exp1 exp2 bdy
------------------ FOR ------------------
------------------ WHILE ------------------
checkStatement env (While expression bdy) =
	case (getExpressionType [] expression env) of
		Right expType ->
			-- si no es una condicion booleana
			if (expType /= TyBool)
			then 
				-- la expresion es incorrecta, chequeo el resto
				case checkBody env bdy of
					-- solo la expresion falla
					Right env -> Left [(Expected TyBool expType)]
					-- ya viene una lista de errores
					Left errsBdy -> Left ([(Expected TyBool expType)] ++ errsBdy)
			else
				-- la expresion es correcta, chequeo el resto
				case checkBody env bdy of
					-- salio todo bien
					Right env -> Right env
					-- ya viene una lista de errores
					Left errsBdy -> Left errsBdy
		Left errsExp -> 
			-- la expresion es incorrecta, chequeo el resto
			case checkBody env bdy of
				Right env -> Left errsExp
				-- ya viene una lista de errores
				Left errsBdy -> Left (errsExp ++ errsBdy)
------------------ WHILE ------------------
------------------ WRITE ------------------
checkStatement env (Write expression) = 
	case (getExpressionType [] expression env) of
		Right expType ->
			if (expType /= TyInt)
			-- lo que se le pasa al writeln no es de tipo entero
			then Left [(Expected TyInt expType)]
			else Right env
		Left errs -> Left errs
------------------ WRITE ------------------
------------------ READ ------------------
checkStatement env (Read name) = 
	-- si la variable esta indefinida a la hora de usarla
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
	case (getExpressionType [] (head exps) env) of
		Right ty ->
			if (length (tail exps) > 0)
			then
				if (ty == TyInt)
				-- si el actual es entero, sigo con el resto
				then checkIntegerArrayIndices (tail exps) env
				else ty
			else ty -- si los indices son enteros devuelvo true
		Left errs -> TyInt -- NO OCURRE, FUCK YOU

-- encuentra el tipo de una expresion
getExpressionType :: [Error] -> Expr -> Env -> Either [Error] Type
-- acceso a arreglo
-- asumiendo que los indices son del mismo tipo y esta todo bien
getExpressionType errs (Var name exps) env
	-- si es arreglo multidimensional, obtiene el tipo recursivamente
	-- no se chequea que la variable este definida por su nombre
	| length exps /= 0 = 
		-- si todos los indices del acceso al arreglo son enteros
		if (checkIntegerArrayIndices exps env == TyInt)
		then
			-- trato de comparar el valor de asignacion con lo que contiene el array
			case (getTypeByName name env) of
				-- si efectivamente es un array
				Just (TyArray ini fin ty) -> Right (getArrayType ty)
				Just (TyInt) -> Left [(NotArray TyInt)] -- no era un array
				Just (TyBool) -> Left [(NotArray TyBool)] -- no era un array
				Nothing -> Left [(Undefined name)] -- puede pasar que se intente a acceder
				-- a algo que ni existe
		-- tiro el tipo de indices usados
		else Left [(Expected TyInt (checkIntegerArrayIndices exps env))]
	-- si es simplemente una referencia a una variable, devuelve su tipo
	| otherwise        = case (getTypeByName name env) of
							Just ty -> Right ty
							Nothing -> Left [(Undefined name)]
getExpressionType errs (IntLit int) env = Right TyInt
getExpressionType errs (BoolLit bool) env = Right TyBool
-- aca no concateno errs, tengo que pasarlo por parametro
getExpressionType errs (Unary uop expr) env = getExpressionType errs expr env
getExpressionType errs (Binary bop exp1 exp2) env
	-- HAY QUE REFINAR ESTO
	| bop == Or || bop == And = --ambas expresiones tienen que ser de tipo logico
		case (getExpressionType [] exp1 env) of
			Right ty1 ->
				case (getExpressionType [] exp2 env) of
					Right ty2 ->
						if (ty1 == TyBool)
						then
							if (ty2 == TyBool)
							then Right TyBool
							else Left (errs ++ [(Expected TyBool ty2)])
						else
							if (ty2 == TyBool)
							then Left (errs ++ [(Expected TyBool ty1)])
							else Left (errs ++ [(Expected TyBool ty1),(Expected TyBool ty2)])
					Left errs2 ->
						if (ty1 == TyBool)
						then Left (errs ++ [(Expected TyBool ty1)] ++ errs2)
						else Left (errs ++ errs2)
			Left errs1 ->
				case (getExpressionType [] exp2 env) of
					Right ty2 ->
						if (ty2 == TyBool)
						then Left (errs ++ errs1)
						else Left (errs ++ errs1 ++ [(Expected TyBool ty2)])
					Left errs2 -> Left (errs ++ errs1 ++ errs2)
	| bop == Equ = --tienen que ser del mismo tipo las expresiones
		case (getExpressionType [] exp1 env) of
			Right ty1 ->
				case (getExpressionType [] exp2 env) of
					Right ty2 ->
						if (ty1 == ty2)
						then Right TyBool
						-- expresiones no comparables
						else Left (errs ++ [(Expected ty1 ty2)])
					Left errs2 -> Left (errs ++ errs2)
			Left errs1 -> 
				case (getExpressionType [] exp2 env) of
					Right ty2 -> Left (errs ++ errs1)
					Left errs2 -> Left (errs ++ errs1 ++ errs2)	
	| bop == Less = -- las dos expresiones deben ser enteras
	-- pero se debe devolver TyBool, no como el caso de las operaciones
	-- enteras binarias
		-- chequeo primera expresion de la operacion binaria
		case (getExpressionType [] exp1 env) of
			Right ty1 ->
				-- chequeo segunda expresion de la operacion binaria
				case (getExpressionType [] exp2 env) of
					Right ty2 ->
						if (ty1 == TyInt)
						then
							if (ty2 == TyInt)
							then Right TyBool
							else Left (errs ++ [(Expected TyInt ty2)])
						else
							if (ty2 == TyInt)
							then Left (errs ++ [(Expected TyInt ty1)])
							else Left (errs ++ [(Expected TyInt ty1), (Expected TyInt ty2)])
					Left errs2 ->
						if (ty1 == TyInt)
						then Left (errs ++ errs2)
						else Left (errs ++ [(Expected TyInt ty1)] ++ errs2)
			Left errs1 ->
				case (getExpressionType [] exp2 env) of
					Right ty2 ->
						if (ty2 == TyInt)
						then Left (errs ++ errs1)
						else Left (errs ++ errs1 ++ [(Expected TyInt ty2)])
					Left errs2 -> Left (errs ++ errs1 ++ errs2)	
	| otherwise = -- operaciones con enteros
		-- chequeo primera expresion de la operacion binaria
		case (getExpressionType [] exp1 env) of
			Right ty1 ->
				-- chequeo segunda expresion de la operacion binaria
				case (getExpressionType [] exp2 env) of
					Right ty2 ->
						if (ty1 == TyInt)
						then
							if (ty2 == TyInt)
							then Right TyInt
							else Left (errs ++ [(Expected TyInt ty2)])
						else
							if (ty2 == TyInt)
							then Left (errs ++ [(Expected TyInt ty1)])
							else Left (errs ++ [(Expected TyInt ty1), (Expected TyInt ty2)])
					Left errs2 ->
						if (ty1 == TyInt)
						then Left (errs ++ errs2)
						else Left (errs ++ [(Expected TyInt ty1)] ++ errs2)
			Left errs1 ->
				case (getExpressionType [] exp2 env) of
					Right ty2 ->
						if (ty2 == TyInt)
						then Left (errs ++ errs1)
						else Left (errs ++ errs1 ++ [(Expected TyInt ty2)])
					Left errs2 -> Left (errs ++ errs1 ++ errs2)

-- true si esta definida
checkVarDefined :: String -> Env -> Bool
checkVarDefined name env = length([name_temp | (name_temp, type_temp) <- env, name_temp == name]) > 0