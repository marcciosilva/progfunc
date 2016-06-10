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
import Control.Monad
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
    | length body == 0 = checkVarDef defs [] []
    | otherwise = case checkVarDef defs [] [] of
                    Right env -> checkBody env body
                    -- si hay error en la declaracion no se
                    -- chequea el cuerpo
                    Left errVarDef -> Left errVarDef


{- ================================ DECLARACION DE VARIABLES ===================================== -}
checkVarDef :: [VarDef] -> [Error] -> Env -> Either [Error] Env
checkVarDef vs errs env
    | length vs /= 0 = case checkSingleVar (head vs) env of
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

{- ============================= Fin chequeo declaración de variables  ===================================== -}



{- =============================================== BODY ==================================================== -}
checkBody :: Env -> [Stmt] -> Either [Error] Env
checkBody env stmts = checkStatements env [] stmts


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
    if((length exps) > 0) 
    then case (getTypeByName name env) of
            -- si efectivamente es un array
        Just (TyArray ini fin ty) -> case (checkIntegerArrayIndices [] exps env) of
										Right typ1 -> 
											case (getExpressionType [] expression env) of
												--si la expresion tiene un tipo asignado
												Right expType ->
												-- chequeo si el tipo de variables que guarda el array
												-- se corresponde con el tipo de la expresion
												-- asumo que si el arreglo es accedido incorrectamente
												-- ni siquiera me fijo en si el tipo de la expresion coincide
													case compare (checkArrayTypeDimensions(TyArray ini fin ty)) (length exps) of
														LT -> Left [(NotArray expType)]
														-- GT -> Left [(ArrayAssignment expType)]
														GT -> Left [(ArrayAssignment ty)]
														EQ -> 
															if ((getArrayType ty) == expType)
															then Right env
															-- chequear si el tipo de la derecha es array
															else Left [(Expected (getArrayType ty) expType)]
												-- si la expresion esta indefinida por ejemplo
												Left errs -> Left errs
										Left err1 -> case (getExpressionType err1 expression env) of
											--si la expresion tiene un tipo asignado
											Right expType ->
											-- chequeo si el tipo de variables que guarda el array
											-- se corresponde con el tipo de la expresion
											-- asumo que si el arreglo es accedido incorrectamente
											-- ni siquiera me fijo en si el tipo de la expresion coincide
												case compare (checkArrayTypeDimensions(TyArray ini fin ty)) (length exps) of
													LT -> Left (err1 ++ [(NotArray expType)])
													-- GT -> Left [(ArrayAssignment expType)]
													GT -> Left (err1 ++ [(ArrayAssignment ty)])
													EQ -> 
														if ((getArrayType ty) == expType)
														then Left err1
														-- chequear si el tipo de la derecha es array
														else Left (err1 ++ [(Expected (getArrayType ty) expType)])
											-- si la expresion esta indefinida por ejemplo
											Left errs -> Left (err1 ++ errs)
        Just (TyInt) -> Left [(NotArray TyInt)] -- no era un array lo de la izquierda
        Just (TyBool) -> Left [(NotArray TyBool)] -- no era un array lo de la izquierda
        Nothing -> Left [(Undefined name)] -- no pasa, dado que length exps > 0; es un array
    -- si es una unica variable
    else
        case (getExpressionType [] expression env) of
            Right expType ->
                -- if (expType /= TyInt && expType /= TyBool)
                -- -- asignacion de un arreglo
                -- -- then Left [(ArrayAssignment expType)]
                -- then Left [(ArrayAssignment expType)]
                -- else
                --     case (getTypeByName name env) of
                --         Just ty ->
                --             if (ty == expType)
                --             then Right env
                --             else Left [(Expected ty expType)]
                --         Nothing -> Left [(Undefined name)]
                -- me fijo si la expresion de la derecha resulta un array
                case (getTypeByName name env) of
                    -- Cuando se intenta hacer una asignación sobre una variable array directamente
                    Just (TyArray ini fin ty) -> Left [(ArrayAssignment (TyArray ini fin ty) )]
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
checkStatement env (For name exp1 exp2 bdy) =  
	-- chequeo si variable de iteracion default esta definida
    if ((checkVarDefined name env) == False)
    then
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
                                    Right env -> Left ([(Undefined name)] ++ [(Expected TyInt exp1Type)] ++ [(Expected TyInt exp2Type)])
                                    -- todo tiene errores
                                    Left errLst -> Left ([(Undefined name)] ++ [(Expected TyInt exp1Type)] ++ [(Expected TyInt exp2Type)] ++ errLst)
                            else 
                                -- falla exp1, no falla exp2, hay que ver el bdy
                                case checkBody env bdy of
                                    Right env -> Left ([(Undefined name)] ++ [(Expected TyInt exp1Type)])
                                    -- fallan exp1 mas el bdy
                                    Left errLst -> Left ([(Undefined name)] ++ [(Expected TyInt exp1Type)] ++ errLst)
                        -- si alguna variable no esta definida en exp2
                        Left errs1 ->
                            -- ya se sabe que falló exp2, y que el exptype de exp1 no matchea
                            case checkBody env bdy of
                                -- el body esta bien pero lo demas tiene errores
                                Right env -> Left ([(Undefined name)] ++ [(Expected TyInt exp1Type)] ++ errs1)
                                -- todo tiene errores
                                Left errLst -> Left ([(Undefined name)] ++ [(Expected TyInt exp1Type)] ++ errs1 ++ errLst)
                else
                    -- aca al menos exp 1 salio bien
                    case (getExpressionType [] exp2 env) of
                        Right exp2Type ->
                            if (exp2Type /= TyInt)
                            then 
                                -- aca falla por lo menos la exp2, y el bdy puede fallar o no
                                case checkBody env bdy of
                                    Right env -> Left ([(Undefined name)] ++ [(Expected TyInt exp2Type)])
                                    -- ya viene una lista de errores
                                    Left errLst -> Left ([(Undefined name)] ++ [(Expected TyInt exp2Type)] ++ errLst)
                            else 
                                -- en esta bifurcacion sale todo bien, o solo falla el body
                                case checkBody env bdy of
                                    -- sale todo bien pero como falla la definicion de i, marchamos
                                    Right env -> Left ([(Undefined name)])
                                    -- ya viene una lista de errores
                                    Left errLst -> Left ([(Undefined name)] ++ errLst)
                        Left errs1 ->
                            -- exp2 esta mal, y exp 1 bien
                            case checkBody env bdy of
                                Right env -> Left ([(Undefined name)] ++ errs1)
                                -- ya viene una lista de errores
                                Left errLst -> Left ([(Undefined name)] ++ errs1 ++ errLst)
            Left errs1 -> --exp1 salio mal de unaLeft errs ++ [err]    
                -- aca al menos exp1 salió mal, hay que ver que pasa con exp2 y bdy
                case (getExpressionType [] exp2 env) of
                    Right exp2Type ->
                        if (exp2Type /= TyInt)
                        then 
                            -- aca falla por lo menos la exp2, y el bdy puede fallar o no
                            case checkBody env bdy of
                                Right env -> Left ([(Undefined name)] ++ errs1 ++ [(Expected TyInt exp2Type)])
                                -- todo tiene errores
                                Left errLst -> Left ([(Undefined name)] ++ errs1 ++ [(Expected TyInt exp2Type)] ++ errLst)
                        else 
                            -- falla exp1, no falla exp2, hay que ver el bdy
                            case checkBody env bdy of
                                Right env -> Left ([(Undefined name)] ++ errs1)
                                -- fallan exp1 mas el bdy
                                Left errLst -> Left ([(Undefined name)] ++ errs1 ++ errLst)
                    -- si alguna variable no esta definida en exp2
                    Left errs2 ->
                        -- ya se sabe que falló exp2, y que el exptype de exp1 no matchea
                        case checkBody env bdy of
                            -- el body esta bien pero lo demas tiene errores
                            Right env -> Left ([(Undefined name)] ++ errs1 ++ errs2)
                            -- todo tiene errores
                            Left errLst -> Left ([(Undefined name)] ++ errs1 ++ errs2 ++ errLst)
    else
        -- chequeo que la variable i sea de tipo entero
        case (getTypeByName name env) of
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
                                                Right env -> Left ([(Expected TyInt exp1Type)] ++ [(Expected TyInt exp2Type)])
                                                -- todo tiene errores
                                                Left errLst -> Left ([(Expected TyInt exp1Type)] ++ [(Expected TyInt exp2Type)] ++ errLst)
                                        else 
                                            -- falla exp1, no falla exp2, hay que ver el bdy
                                            case checkBody env bdy of
                                                Right env -> Left ([(Expected TyInt exp1Type)])
                                                -- fallan exp1 mas el bdy
                                                Left errLst -> Left ([(Expected TyInt exp1Type)] ++ errLst)
                                    -- si alguna variable no esta definida en exp2
                                    Left errs1 ->
                                        -- ya se sabe que falló exp2, y que el exptype de exp1 no matchea
                                        case checkBody env bdy of
                                            -- el body esta bien pero lo demas tiene errores
                                            Right env -> Left ([(Expected TyInt exp1Type)] ++ errs1)
                                            -- todo tiene errores
                                            Left errLst -> Left ([(Expected TyInt exp1Type)] ++ errs1 ++ errLst)
                            else
                                -- aca al menos exp 1 salio bien
                                case (getExpressionType [] exp2 env) of
                                    Right exp2Type ->
                                        if (exp2Type /= TyInt)
                                        then 
                                            -- aca falla por lo menos la exp2, y el bdy puede fallar o no
                                            case checkBody env bdy of
                                                Right env -> Left ([(Expected TyInt exp2Type)])
                                                -- ya viene una lista de errores
                                                Left errLst -> Left ([(Expected TyInt exp2Type)] ++ errLst)
                                        else 
                                            -- en esta bifurcacion sale todo bien, o solo falla el body
                                            case checkBody env bdy of
                                                -- sale todo bien
                                                Right env -> Right env
                                                -- ya viene una lista de errores
                                                Left errLst -> Left errLst
                                    Left errs1 ->
                                        -- exp2 esta mal, y exp 1 bien
                                        case checkBody env bdy of
                                            Right env -> Left errs1
                                            -- ya viene una lista de errores
                                            Left errLst -> Left (errs1 ++ errLst)
                        Left errs1 -> --exp1 salio mal de unaLeft errs ++ errs1    
                            -- aca al menos exp1 salió mal, hay que ver que pasa con exp2 y bdy
                            case (getExpressionType [] exp2 env) of
                                Right exp2Type ->
                                    if (exp2Type /= TyInt)
                                    then 
                                        -- aca falla por lo menos la exp2, y el bdy puede fallar o no
                                        case checkBody env bdy of
                                            Right env -> Left (errs1 ++ [(Expected TyInt exp2Type)])
                                            -- todo tiene errores
                                            Left errLst -> Left (errs1 ++ [(Expected TyInt exp2Type)] ++ errLst)
                                    else 
                                        -- falla exp1, no falla exp2, hay que ver el bdy
                                        case checkBody env bdy of
                                            Right env -> Left (errs1)
                                            -- fallan exp1 mas el bdy
                                            Left errLst -> Left (errs1 ++ errLst)
                                -- si alguna variable no esta definida en exp2
                                Left errs2 ->
                                    -- ya se sabe que falló exp2, y que el exptype de exp1 no matchea
                                    case checkBody env bdy of
                                        -- el body esta bien pero lo demas tiene errores
                                        Right env -> Left (errs1 ++ errs2)
                                        -- todo tiene errores
                                        Left errLst -> Left (errs1 ++ errs2 ++ errLst)
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
                                                Right env -> Left ([(Expected TyInt ty)] ++ [(Expected TyInt exp1Type)] ++ [(Expected TyInt exp2Type)])
                                                -- todo tiene errores
                                                Left errLst -> Left ([(Expected TyInt ty)] ++ [(Expected TyInt exp1Type)] ++ [(Expected TyInt exp2Type)] ++ errLst)
                                        else 
                                            -- falla exp1, no falla exp2, hay que ver el bdy
                                            case checkBody env bdy of
                                                Right env -> Left ([(Expected TyInt ty)] ++ [(Expected TyInt exp1Type)])
                                                -- fallan exp1 mas el bdy
                                                Left errLst -> Left ([(Expected TyInt ty)] ++ [(Expected TyInt exp1Type)] ++ errLst)
                                    -- si alguna variable no esta definida en exp2
                                    Left errs1 ->
                                        -- ya se sabe que falló exp2, y que el exptype de exp1 no matchea
                                        case checkBody env bdy of
                                            -- el body esta bien pero lo demas tiene errores
                                            Right env -> Left ([(Expected TyInt ty)] ++ [(Expected TyInt exp1Type)] ++ errs1)
                                            -- todo tiene errores
                                            Left errLst -> Left ([(Expected TyInt ty)] ++ [(Expected TyInt exp1Type)] ++ errs1 ++ errLst)
                            else
                                -- aca al menos exp 1 salio bien
                                case (getExpressionType [] exp2 env) of
                                    Right exp2Type ->
                                        if (exp2Type /= TyInt)
                                        then 
                                            -- aca falla por lo menos la exp2, y el bdy puede fallar o no
                                            case checkBody env bdy of
                                                Right env -> Left ([(Expected TyInt ty)] ++ [(Expected TyInt exp2Type)])
                                                -- ya viene una lista de errores
                                                Left errLst -> Left ([(Expected TyInt ty)] ++ [(Expected TyInt exp2Type)] ++ errLst)
                                        else 
                                            -- en esta bifurcacion sale todo bien, o solo falla el body
                                            case checkBody env bdy of
                                                -- sale todo bien pero como falla la definicion de i, marchamos
                                                Right env -> Left ([(Expected TyInt ty)])
                                                -- ya viene una lista de errores
                                                Left errLst -> Left ([(Expected TyInt ty)] ++ errLst)
                                    Left errs1 ->
                                        -- exp2 esta mal, y exp 1 bien
                                        case checkBody env bdy of
                                            Right env -> Left ([(Expected TyInt ty)] ++ errs1)
                                            -- ya viene una lista de errores
                                            Left errLst -> Left ([(Expected TyInt ty)] ++ errs1 ++ errLst)
                        Left errs1 -> --exp1 salio mal de unaLeft errs ++ [err]    
                            -- aca al menos exp1 salió mal, hay que ver que pasa con exp2 y bdy
                            case (getExpressionType [] exp2 env) of
                                Right exp2Type ->
                                    if (exp2Type /= TyInt)
                                    then 
                                        -- aca falla por lo menos la exp2, y el bdy puede fallar o no
                                        case checkBody env bdy of
                                            Right env -> Left ([(Expected TyInt ty)] ++ errs1 ++ [(Expected TyInt exp2Type)])
                                            -- todo tiene errores
                                            Left errLst -> Left ([(Expected TyInt ty)] ++ errs1 ++ [(Expected TyInt exp2Type)] ++ errLst)
                                    else 
                                        -- falla exp1, no falla exp2, hay que ver el bdy
                                        case checkBody env bdy of
                                            Right env -> Left ([(Expected TyInt ty)] ++ errs1)
                                            -- fallan exp1 mas el bdy
                                            Left errLst -> Left ([(Expected TyInt ty)] ++ errs1 ++ errLst)
                                -- si alguna variable no esta definida en exp2
                                Left errs2 ->
                                    -- ya se sabe que falló exp2, y que el exptype de exp1 no matchea
                                    case checkBody env bdy of
                                        -- el body esta bien pero lo demas tiene errores
                                        Right env -> Left ([(Expected TyInt ty)] ++ errs1 ++ errs2)
                                        -- todo tiene errores
                                        Left errLst -> Left ([(Expected TyInt ty)] ++ errs1 ++ errs2 ++ errLst)
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
    case (getExpressionTypeSimple expression env) of
        Just expType ->
            if (expType /= TyInt)
            -- lo que se le pasa al writeln no es de tipo entero
            then case (getExpressionType [] expression env) of 
					  Right exp1Type -> Left [(Expected TyInt exp1Type)]
					  Left errs -> Left errs
            else Right env
        Nothing -> Left [(Undefined (getExpressionVarName expression env))]
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

checkIntegerArrayIndices :: [Error] -> [Expr] -> Env -> Either [Error] Type
checkIntegerArrayIndices errs exps env =
	case (getExpressionType errs (head exps) env) of
		Right ty ->
			if (length (tail exps) > 0)
			then
				if (ty == TyInt)
				-- si el actual es entero, sigo con el resto
				then checkIntegerArrayIndices errs (tail exps) env
				else checkIntegerArrayIndices (errs ++ [(Expected TyInt ty)]) (tail exps) env
			else 
				if (ty == TyInt) -- si los indices son enteros devuelvo true
				-- si el actual es entero, sigo con el resto
				then if (length errs == 0)
						then Right TyInt
						else Left errs
				else Left (errs ++ [(Expected TyInt ty)])
		Left errsExp ->
			if (length (tail exps) > 0)
			then checkIntegerArrayIndices (errs ++ errsExp) (tail exps) env
			else Left (errs ++ errsExp)


-- encuentra el tipo de una expresion
getExpressionType :: [Error] -> Expr -> Env -> Either [Error] Type
-- acceso a arreglo
-- asumiendo que los indices son del mismo tipo y esta todo bien
getExpressionType errs (Var name exps) env
    -- si es arreglo multidimensional, obtiene el tipo recursivamente
    -- no se chequea que la variable este definida por su nombre
    | length exps /= 0 = 
        -- si todos los indices del acceso al arreglo son enteros
        case (checkIntegerArrayIndices errs exps env) of
			Right typ -> if(typ == TyInt)
							then
								-- trato de comparar el valor de asignacion con lo que contiene el array
								case (getTypeByName name env) of
									-- si efectivamente es un array
									Just (TyArray ini fin ty) -> 
										case compare (checkArrayTypeDimensions(TyArray ini fin ty)) (length exps) of
											LT -> 	Left (errs ++ [(NotArray typ)])
											GT -> 	Left (errs ++ [(ArrayAssignment ty)])
											EQ -> 	if ((getArrayType ty) == typ)
													then Right typ
													-- chequear si el tipo de la derecha es array
													else 
														case (getExpressionType [] (last exps) env) of
															Right typ3 -> Left (errs ++ [(Expected (getArrayType ty) typ3)])
															Left err3 -> Left (errs ++ err3)
									-- de acuerdo a la profundidad del acceso al array, hay que devolver un tipo acorde
									-- que puede ser un array perfectamente
									-- habria que hacer una funcion donde me pasen la cantidad de accesos que se hacen,
									-- dada por (length exps), para determinar el tipo del array (pudiendo ser array of array...)
									-- TODO:
										-- case compare (checkArrayTypeDimensions(TyArray ini fin ty)) (length exps) of
										--     LT -> Left [(NotArray expType)]
										--     GT -> 
										-- LLAMADO A FUNCION NUEVA
										--     EQ -> 
										--         if ((getArrayType ty) == expType)
										--         then Right env
										--         -- chequear si el tipo de la derecha es array
										--         else Left [(Expected (getArrayType ty) expType)]
									Just (TyInt) -> Left (errs ++ [(NotArray TyInt)]) -- no era un array
									Just (TyBool) -> Left (errs ++ [(NotArray TyBool)]) -- no era un array
									Nothing -> Left (errs ++ [(Undefined name)]) -- puede pasar que se intente a acceder
									-- a algo que ni existe
							-- tiro el tipo de indices usados
							else Left (errs ++ [(Expected TyInt typ)])
			Left errInt -> Left errInt

	-- si es simplemente una referencia a una variable, devuelve su tipo
    | otherwise        = case (getTypeByName name env) of
                            Just ty -> Right ty
                            Nothing -> Left (errs ++ [(Undefined name)])


{-================ CHEQUEO DE LITERALES ===============-}
getExpressionType errs (IntLit int) env = Right TyInt
getExpressionType errs (BoolLit bool) env = Right TyBool


{-================ CHEQUEO DE EXPRESIONES UNARIAS ===============-} 
getExpressionType errs (Unary uop expr) env
    | uop == Not = -- asumiendo que not debe aplicarse a expresiones logicas unicamente
        case (getExpressionType [] expr env) of
            Right ty ->
                if (ty == TyBool)
                then Right TyBool
                else Left (errs ++ [(Expected TyBool ty)])
            Left errsExpr -> Left (errs ++ errsExpr)
    | uop == Neg = -- asumiendo que neg debe aplicarse a expresiones enteras unicamente
        case (getExpressionType [] expr env) of
            Right ty ->
                if (ty == TyInt)
                then Right TyInt
                else Left (errs ++ [(Expected TyInt ty)])
            Left errsExpr -> Left (errs ++ errsExpr)        

            
{-================ CHEQUEO DE EXPRESIONES BINARIAS ===============-}            
getExpressionType errs (Binary bop exp1 exp2) env
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
                        then Left (errs ++ errs2)
                        else Left (errs ++ errs2)
            Left errs1 -> Left (errs ++ errs1)

   | bop == Less || bop == Equ = -- las dos expresiones deben ser enteras
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
                    Left errs2 -> Left (errs ++ errs2)
            Left errs1 -> Left (errs ++ errs1)

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
                    Left errs2 -> Left (errs ++ errs2)
            Left errs1 -> Left (errs ++ errs1)


-- true si esta definida
checkVarDefined :: String -> Env -> Bool
checkVarDefined name env = length([name_temp | (name_temp, type_temp) <- env, name_temp == name]) > 0


-- devuelve la dimensión del tipo pasado 
-- x[2] -> 1, x[1][2] -> 2, x -> 0
checkArrayTypeDimensions :: Type -> Int
checkArrayTypeDimensions(TyArray ini fin ty) = (checkArrayTypeDimensions ty) + 1
checkArrayTypeDimensions(TyInt) = 0
checkArrayTypeDimensions(TyBool) = 0


-- devuelve el nombre de la variable en la expresión. No asegura que esté definida.
getExpressionVarName :: Expr -> Env -> String
getExpressionVarName (Var name exps) env = name


-- devuelve el tipo asignado a la variable de la expresión en el ambiente
getExpressionTypeSimple :: Expr -> Env -> Maybe Type
getExpressionTypeSimple (Var name exps) env =
	if (checkVarDefined name env)
	then Just (head [b | (a,b) <- env, (a==name)])
	else Nothing
