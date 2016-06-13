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

    -- se agregaron todas las vars a env pero hubo errores
    | length errs /= 0 = Left errs            
    
    -- se agregaron todas las vars a env sin errores                
    | otherwise = Right env    

    
checkSingleVar :: VarDef -> Env -> Either Error Env
checkSingleVar (VarDef name type1) env 
    -- Variable repetida
    | containsVariable env (name, type1) = Left (Duplicated name)
    
    -- agrego variable al final para declararlas en orden de ocurrencia en el .pas
    | otherwise = Right (env ++ [(name, type1)])

    
-- chequea si la variable ya esta definida en env
containsVariable :: Env -> (Name,Type) -> Bool
-- si se repite el identificador, la lista queda de largo > 0
-- genero lista con variables que tienen el mismo nombre que la del parametro
-- si el largo es mayor a cero, esta duplicada
containsVariable vs (name,ty) = length ([(name_temp, type_temp)| (name_temp, type_temp) <- vs, name_temp == name]) > 0

{- ============================= Fin chequeo declaración de variables  ===================================== -}



{- =============================================== BODY ==================================================== -}
checkBody :: Env -> [Stmt] -> Either [Error] Env
checkBody env stmts = checkStatements env [] stmts


checkStatements :: Env -> [Error] -> [Stmt] -> Either [Error] Env
checkStatements env errs stmts
    | (length stmts) /= 0 = case checkStatement env (head stmts) of
                                -- agrego error al final para mostrar los 
                                -- errores en orden de ocurrencia
                                Just errLst -> checkStatements env (errs ++ errLst) (tail stmts)
                                Nothing -> checkStatements env errs (tail stmts)
    -- se terminaron de chequear los stms y hubo errores
    | length errs /= 0 = Left errs            
    
    -- se terminaron de chequear los stms sin errores                
    | otherwise = Right env    


checkStatement :: Env -> Stmt -> Maybe [Error]
------------------ ASIGNACION DE VARIABLES ------------------
checkStatement env (Asig name exps expression)
    | (length exps) > 0 =
-- chequeo que la variable esté definida
        case (getTypeByName name env) of
            Just ty ->
                -- si todos los indices del acceso al arreglo son enteros
                case (checkIntegerArrayIndices exps env) of
                    Right typ -> 
                        -- trato de comparar el valor de asignacion con lo que contiene el array
                        if (isArray ty) then
                            -- si efectivamente es un array
                                case compare (checkArrayTypeDimensions ty) (length exps) of
                                    LT ->     Just [(NotArray typ)]
                                    GT ->     Just [(ArrayAssignment (getLastArrayType ty))]
                                    EQ ->
                                        -- procedo a chequar la expresión de la derecha
                                        case (getExpressionType expression env) of
                                            Right expType ->
                                                    if ((getArrayType ty) == expType)
                                                    then Nothing
                                                    else Just [(Expected (getArrayType ty) expType)]
                                            Left errExp -> Just errExp
                        else Just [(NotArray ty)]
                    Left errInt ->     if (isErrorUndefined (last errInt))
                                    then Just errInt
                                    else case (getExpressionType expression env) of
                                            Right expType -> Just errInt
                                            Left errExp -> Just (errInt ++ errExp)
            Nothing ->     -- procedo a chequar la expresión de la derecha
                        case (getExpressionType expression env) of
                            Right expType -> Just [(Undefined name)]
                            Left errExp -> Just ([(Undefined name)] ++ errExp)
        
    | otherwise =     case (getTypeByName name env) of
                        Just ty ->     if (isArray ty) 
                                    then Just [(ArrayAssignment ty)]
                                    else case (getExpressionType expression env) of
                                            Right expType ->
                                                if (ty == expType)
                                                then Nothing
                                                else Just [(Expected ty expType)]
                                            Left errExp -> Just errExp
                        Nothing ->     -- procedo a chequar la expresión de la derecha
                                    case (getExpressionType expression env) of
                                        Right expType -> Just [(Undefined name)]
                                        Left errExp -> Just ([(Undefined name)] ++ errExp)
------------------ ASIGNACION DE VARIABLES ------------------
------------------ IF ------------------
checkStatement env (If expression bdy1 bdy2) = 
    case (getExpressionType expression env) of
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
                            Right env -> Just [(Expected TyBool expType)]
                            -- ya viene una lista de errores
                            Left errsBdy2 -> Just ([(Expected TyBool expType)] ++ errsBdy2)
                    Left errsBdy1 ->
                        -- salio mal el bdy 1, tengo que chequear el 2 igual
                        case checkBody env bdy2 of
                            Right env -> Just ([(Expected TyBool expType)] ++ errsBdy1)
                            -- sale todo mal
                            Left errsBdy2 -> Just ([(Expected TyBool expType)] ++ errsBdy1 ++ errsBdy2)
            else
                -- la expresion es correcta, chequeo el resto
                case checkBody env bdy1 of
                    Right env ->
                        -- chequeo del cuerpo del else
                        case checkBody env bdy2 of
                            -- salio todo bien
                            Right env -> Nothing
                            -- ya viene una lista de errores
                            Left errsBdy2 -> Just errsBdy2
                    Left errsBdy1 ->
                        -- salio mal el bdy 1, tengo que chequear el 2 igual
                        case checkBody env bdy2 of
                            Right env -> Just errsBdy1
                            -- ya viene una lista de errores
                            Left errsBdy2 -> Just (errsBdy1 ++ errsBdy2)
        Left errsExp -> 
            -- la expresion es incorrecta, chequeo el resto
            case checkBody env bdy1 of
                Right env ->
                    -- chequeo del cuerpo del else
                    case checkBody env bdy2 of
                        Right env -> Just errsExp
                        -- ya viene una lista de errores
                        Left errsBdy2 -> Just (errsExp ++ errsBdy2)
                Left errsBdy1 ->
                    -- salio mal el bdy 1, tengo que chequear el 2 igual
                    case checkBody env bdy2 of
                        Right env -> Just (errsExp ++ errsBdy1)
                        -- sale todo mal
                        Left errsBdy2 -> Just (errsExp ++ errsBdy1 ++ errsBdy2)
------------------ IF ------------------
------------------ FOR ------------------
checkStatement env (For name exp1 exp2 bdy) =  
    -- chequeo si variable de iteracion default esta definida
    if ((checkVarDefined name env) == False)
    then
        case (getExpressionType exp1 env) of
            Right exp1Type ->
                if (exp1Type /= TyInt)
                then
                    -- aca al menos exp1 salió mal, hay que ver que pasa con exp2 y bdy
                    case (getExpressionType exp2 env) of
                        Right exp2Type ->
                            if (exp2Type /= TyInt)
                            then 
                                -- aca falla por lo menos la exp2, y el bdy puede fallar o no
                                case checkBody env bdy of
                                    Right env -> Just ([(Undefined name)] ++ [(Expected TyInt exp1Type)] ++ [(Expected TyInt exp2Type)])
                                    -- todo tiene errores
                                    Left errLst -> Just ([(Undefined name)] ++ [(Expected TyInt exp1Type)] ++ [(Expected TyInt exp2Type)] ++ errLst)
                            else 
                                -- falla exp1, no falla exp2, hay que ver el bdy
                                case checkBody env bdy of
                                    Right env -> Just ([(Undefined name)] ++ [(Expected TyInt exp1Type)])
                                    -- fallan exp1 mas el bdy
                                    Left errLst -> Just ([(Undefined name)] ++ [(Expected TyInt exp1Type)] ++ errLst)
                        -- si alguna variable no esta definida en exp2
                        Left errs1 ->
                            -- ya se sabe que falló exp2, y que el exptype de exp1 no matchea
                            case checkBody env bdy of
                                -- el body esta bien pero lo demas tiene errores
                                Right env -> Just ([(Undefined name)] ++ [(Expected TyInt exp1Type)] ++ errs1)
                                -- todo tiene errores
                                Left errLst -> Just ([(Undefined name)] ++ [(Expected TyInt exp1Type)] ++ errs1 ++ errLst)
                else
                    -- aca al menos exp 1 salio bien
                    case (getExpressionType exp2 env) of
                        Right exp2Type ->
                            if (exp2Type /= TyInt)
                            then 
                                -- aca falla por lo menos la exp2, y el bdy puede fallar o no
                                case checkBody env bdy of
                                    Right env -> Just ([(Undefined name)] ++ [(Expected TyInt exp2Type)])
                                    -- ya viene una lista de errores
                                    Left errLst -> Just ([(Undefined name)] ++ [(Expected TyInt exp2Type)] ++ errLst)
                            else 
                                -- en esta bifurcacion sale todo bien, o solo falla el body
                                case checkBody env bdy of
                                    -- sale todo bien pero como falla la definicion de i, marchamos
                                    Right env -> Just ([(Undefined name)])
                                    -- ya viene una lista de errores
                                    Left errLst -> Just ([(Undefined name)] ++ errLst)
                        Left errs1 ->
                            -- exp2 esta mal, y exp 1 bien
                            case checkBody env bdy of
                                Right env -> Just ([(Undefined name)] ++ errs1)
                                -- ya viene una lista de errores
                                Left errLst -> Just ([(Undefined name)] ++ errs1 ++ errLst)
            Left errs1 -> --exp1 salio mal de unaLeft errs ++ [err]    
                -- aca al menos exp1 salió mal, hay que ver que pasa con exp2 y bdy
                case (getExpressionType exp2 env) of
                    Right exp2Type ->
                        if (exp2Type /= TyInt)
                        then 
                            -- aca falla por lo menos la exp2, y el bdy puede fallar o no
                            case checkBody env bdy of
                                Right env -> Just ([(Undefined name)] ++ errs1 ++ [(Expected TyInt exp2Type)])
                                -- todo tiene errores
                                Left errLst -> Just ([(Undefined name)] ++ errs1 ++ [(Expected TyInt exp2Type)] ++ errLst)
                        else 
                            -- falla exp1, no falla exp2, hay que ver el bdy
                            case checkBody env bdy of
                                Right env -> Just ([(Undefined name)] ++ errs1)
                                -- fallan exp1 mas el bdy
                                Left errLst -> Just ([(Undefined name)] ++ errs1 ++ errLst)
                    -- si alguna variable no esta definida en exp2
                    Left errs2 ->
                        -- ya se sabe que falló exp2, y que el exptype de exp1 no matchea
                        case checkBody env bdy of
                            -- el body esta bien pero lo demas tiene errores
                            Right env -> Just ([(Undefined name)] ++ errs1 ++ errs2)
                            -- todo tiene errores
                            Left errLst -> Just ([(Undefined name)] ++ errs1 ++ errs2 ++ errLst)
    else
        -- chequeo que la variable i sea de tipo entero
        case (getTypeByName name env) of
            Just ty ->
                if (ty == TyInt)
                then
                    -- esta bien definida i como entero
                    case (getExpressionType exp1 env) of
                        Right exp1Type ->
                            if (exp1Type /= TyInt)
                            then
                                -- aca al menos exp1 salió mal, hay que ver que pasa con exp2 y bdy
                                case (getExpressionType exp2 env) of
                                    Right exp2Type ->
                                        if (exp2Type /= TyInt)
                                        then 
                                            -- aca falla por lo menos la exp2, y el bdy puede fallar o no
                                            case checkBody env bdy of
                                                Right env -> Just ([(Expected TyInt exp1Type)] ++ [(Expected TyInt exp2Type)])
                                                -- todo tiene errores
                                                Left errLst -> Just ([(Expected TyInt exp1Type)] ++ [(Expected TyInt exp2Type)] ++ errLst)
                                        else 
                                            -- falla exp1, no falla exp2, hay que ver el bdy
                                            case checkBody env bdy of
                                                Right env -> Just ([(Expected TyInt exp1Type)])
                                                -- fallan exp1 mas el bdy
                                                Left errLst -> Just ([(Expected TyInt exp1Type)] ++ errLst)
                                    -- si alguna variable no esta definida en exp2
                                    Left errs1 ->
                                        -- ya se sabe que falló exp2, y que el exptype de exp1 no matchea
                                        case checkBody env bdy of
                                            -- el body esta bien pero lo demas tiene errores
                                            Right env -> Just ([(Expected TyInt exp1Type)] ++ errs1)
                                            -- todo tiene errores
                                            Left errLst -> Just ([(Expected TyInt exp1Type)] ++ errs1 ++ errLst)
                            else
                                -- aca al menos exp 1 salio bien
                                case (getExpressionType exp2 env) of
                                    Right exp2Type ->
                                        if (exp2Type /= TyInt)
                                        then 
                                            -- aca falla por lo menos la exp2, y el bdy puede fallar o no
                                            case checkBody env bdy of
                                                Right env -> Just ([(Expected TyInt exp2Type)])
                                                -- ya viene una lista de errores
                                                Left errLst -> Just ([(Expected TyInt exp2Type)] ++ errLst)
                                        else 
                                            -- en esta bifurcacion sale todo bien, o solo falla el body
                                            case checkBody env bdy of
                                                -- sale todo bien
                                                Right env -> Nothing
                                                -- ya viene una lista de errores
                                                Left errLst -> Just errLst
                                    Left errs1 ->
                                        -- exp2 esta mal, y exp 1 bien
                                        case checkBody env bdy of
                                            Right env -> Just errs1
                                            -- ya viene una lista de errores
                                            Left errLst -> Just (errs1 ++ errLst)
                        Left errs1 -> --exp1 salio mal de unaLeft errs ++ errs1    
                            -- aca al menos exp1 salió mal, hay que ver que pasa con exp2 y bdy
                            case (getExpressionType exp2 env) of
                                Right exp2Type ->
                                    if (exp2Type /= TyInt)
                                    then 
                                        -- aca falla por lo menos la exp2, y el bdy puede fallar o no
                                        case checkBody env bdy of
                                            Right env -> Just (errs1 ++ [(Expected TyInt exp2Type)])
                                            -- todo tiene errores
                                            Left errLst -> Just (errs1 ++ [(Expected TyInt exp2Type)] ++ errLst)
                                    else 
                                        -- falla exp1, no falla exp2, hay que ver el bdy
                                        case checkBody env bdy of
                                            Right env -> Just (errs1)
                                            -- fallan exp1 mas el bdy
                                            Left errLst -> Just (errs1 ++ errLst)
                                -- si alguna variable no esta definida en exp2
                                Left errs2 ->
                                    -- ya se sabe que falló exp2, y que el exptype de exp1 no matchea
                                    case checkBody env bdy of
                                        -- el body esta bien pero lo demas tiene errores
                                        Right env -> Just (errs1 ++ errs2)
                                        -- todo tiene errores
                                        Left errLst -> Just (errs1 ++ errs2 ++ errLst)
                else
                    -- le paso una lista vacia para errores, porque errs igual la concateno despues
                    case (getExpressionType exp1 env) of
                        Right exp1Type ->
                            if (exp1Type /= TyInt)
                            then
                                -- aca al menos exp1 salió mal, hay que ver que pasa con exp2 y bdy
                                case (getExpressionType exp2 env) of
                                    Right exp2Type ->
                                        if (exp2Type /= TyInt)
                                        then 
                                            -- aca falla por lo menos la exp2, y el bdy puede fallar o no
                                            case checkBody env bdy of
                                                Right env -> Just ([(Expected TyInt ty)] ++ [(Expected TyInt exp1Type)] ++ [(Expected TyInt exp2Type)])
                                                -- todo tiene errores
                                                Left errLst -> Just ([(Expected TyInt ty)] ++ [(Expected TyInt exp1Type)] ++ [(Expected TyInt exp2Type)] ++ errLst)
                                        else 
                                            -- falla exp1, no falla exp2, hay que ver el bdy
                                            case checkBody env bdy of
                                                Right env -> Just ([(Expected TyInt ty)] ++ [(Expected TyInt exp1Type)])
                                                -- fallan exp1 mas el bdy
                                                Left errLst -> Just ([(Expected TyInt ty)] ++ [(Expected TyInt exp1Type)] ++ errLst)
                                    -- si alguna variable no esta definida en exp2
                                    Left errs1 ->
                                        -- ya se sabe que falló exp2, y que el exptype de exp1 no matchea
                                        case checkBody env bdy of
                                            -- el body esta bien pero lo demas tiene errores
                                            Right env -> Just ([(Expected TyInt ty)] ++ [(Expected TyInt exp1Type)] ++ errs1)
                                            -- todo tiene errores
                                            Left errLst -> Just ([(Expected TyInt ty)] ++ [(Expected TyInt exp1Type)] ++ errs1 ++ errLst)
                            else
                                -- aca al menos exp 1 salio bien
                                case (getExpressionType exp2 env) of
                                    Right exp2Type ->
                                        if (exp2Type /= TyInt)
                                        then 
                                            -- aca falla por lo menos la exp2, y el bdy puede fallar o no
                                            case checkBody env bdy of
                                                Right env -> Just ([(Expected TyInt ty)] ++ [(Expected TyInt exp2Type)])
                                                -- ya viene una lista de errores
                                                Left errLst -> Just ([(Expected TyInt ty)] ++ [(Expected TyInt exp2Type)] ++ errLst)
                                        else 
                                            -- en esta bifurcacion sale todo bien, o solo falla el body
                                            case checkBody env bdy of
                                                -- sale todo bien pero como falla la definicion de i, marchamos
                                                Right env -> Just ([(Expected TyInt ty)])
                                                -- ya viene una lista de errores
                                                Left errLst -> Just ([(Expected TyInt ty)] ++ errLst)
                                    Left errs1 ->
                                        -- exp2 esta mal, y exp 1 bien
                                        case checkBody env bdy of
                                            Right env -> Just ([(Expected TyInt ty)] ++ errs1)
                                            -- ya viene una lista de errores
                                            Left errLst -> Just ([(Expected TyInt ty)] ++ errs1 ++ errLst)
                        Left errs1 -> --exp1 salio mal de unaLeft errs ++ [err]    
                            -- aca al menos exp1 salió mal, hay que ver que pasa con exp2 y bdy
                            case (getExpressionType exp2 env) of
                                Right exp2Type ->
                                    if (exp2Type /= TyInt)
                                    then 
                                        -- aca falla por lo menos la exp2, y el bdy puede fallar o no
                                        case checkBody env bdy of
                                            Right env -> Just ([(Expected TyInt ty)] ++ errs1 ++ [(Expected TyInt exp2Type)])
                                            -- todo tiene errores
                                            Left errLst -> Just ([(Expected TyInt ty)] ++ errs1 ++ [(Expected TyInt exp2Type)] ++ errLst)
                                    else 
                                        -- falla exp1, no falla exp2, hay que ver el bdy
                                        case checkBody env bdy of
                                            Right env -> Just ([(Expected TyInt ty)] ++ errs1)
                                            -- fallan exp1 mas el bdy
                                            Left errLst -> Just ([(Expected TyInt ty)] ++ errs1 ++ errLst)
                                -- si alguna variable no esta definida en exp2
                                Left errs2 ->
                                    -- ya se sabe que falló exp2, y que el exptype de exp1 no matchea
                                    case checkBody env bdy of
                                        -- el body esta bien pero lo demas tiene errores
                                        Right env -> Just ([(Expected TyInt ty)] ++ errs1 ++ errs2)
                                        -- todo tiene errores
                                        Left errLst -> Just ([(Expected TyInt ty)] ++ errs1 ++ errs2 ++ errLst)
------------------ FOR ------------------
------------------ WHILE ------------------
checkStatement env (While expression bdy) =
    case (getExpressionType expression env) of
        Right expType ->
            -- si no es una condicion booleana
            if (expType /= TyBool)
            then 
                -- la expresion es incorrecta, chequeo el resto
                case checkBody env bdy of
                    -- solo la expresion falla
                    Right env -> Just [(Expected TyBool expType)]
                    -- ya viene una lista de errores
                    Left errsBdy -> Just ([(Expected TyBool expType)] ++ errsBdy)
            else
                -- la expresion es correcta, chequeo el resto
                case checkBody env bdy of
                    -- salio todo bien
                    Right env -> Nothing
                    -- ya viene una lista de errores
                    Left errsBdy -> Just errsBdy
        Left errsExp -> 
            -- la expresion es incorrecta, chequeo el resto
            case checkBody env bdy of
                Right env -> Just errsExp
                -- ya viene una lista de errores
                Left errsBdy -> Just (errsExp ++ errsBdy)
------------------ WHILE ------------------
------------------ WRITE ------------------
checkStatement env (Write expression) = 
    case (getExpressionType expression env) of
        Right ty ->
            if (ty /= TyInt)
            -- lo que se le pasa al writeln no es de tipo entero
            then Just [(Expected TyInt ty)]
            else Nothing
        Left errsExp -> Just errsExp
------------------ WRITE ------------------
------------------ READ ------------------
checkStatement env (Read name) = 
    -- si la variable esta indefinida a la hora de usarla
    case (getTypeByName name env) of
        Just ty -> 
            if (ty /= TyInt)
            -- lo que se le pasa al writeln no es de tipo entero
            then Just [(Expected TyInt ty)]
            else Nothing
        Nothing -> Just [(Undefined name)]
------------------ READ ------------------
-- devuelve el tipo final de una variable, como se obtuvo de una expresión, y no de acuerdo al ambiente
getArrayType :: Type -> Type
getArrayType(TyInt) = TyInt
getArrayType(TyBool) = TyBool
getArrayType(TyArray ini fin ty) = getArrayType ty


isTypeSimple :: Type -> Bool
isTypeSimple TyInt = True
isTypeSimple TyBool = True
isTypeSimple (TyArray ini fin ty) = False


getLastArrayType :: Type -> Type
getLastArrayType (TyArray ini fin ty) = if(isTypeSimple ty)
                                        then (TyArray ini fin ty)
                                        else getLastArrayType ty


-- devuelve el tipo de una variable si esta se encuentra definida en el ambiente (sino nada)
getTypeByName :: String -> Env -> Maybe Type
getTypeByName name env = lookup name env


-- chequea que las expresiones de los indices de un array sean correctas, y frena al primer error
checkIntegerArrayIndices :: [Expr] -> Env -> Either [Error] Type
checkIntegerArrayIndices exps env =
    case (getExpressionType (head exps) env) of
        Right ty ->
            if (ty == TyInt)
            then     if (length (tail exps) > 0)
                    then checkIntegerArrayIndices (tail exps) env
                    else Right TyInt
            else Left [(Expected TyInt ty)]
        Left errsExp -> Left errsExp


                    
getArrayTypeByDim :: Type -> Int -> Either [Error] Type
getArrayTypeByDim (TyArray ini fin ty) x
    | x == 0 = Right (TyArray ini fin ty)
    | otherwise = getArrayTypeByDim ty (x-1)


-- encuentra el tipo de una expresion
getExpressionType :: Expr -> Env -> Either [Error] Type
-- acceso a arreglo
-- asumiendo que los indices son del mismo tipo y esta todo bien
getExpressionType (Var name exps) env
    -- caso en que la expresión tiene forma de un arreglo
    | length exps /= 0 =
        -- chequeo que la variable esté definida
        case (getTypeByName name env) of
            Just ty ->
                -- si todos los indices del acceso al arreglo son enteros
                case (checkIntegerArrayIndices exps env) of
                    Right typ -> 
                        if (isArray ty) then
                            -- si efectivamente es un array
                                case compare (checkArrayTypeDimensions ty) (length exps) of
                                    LT ->     getArrayTypeByDim ty (length exps)
                                    GT ->     getArrayTypeByDim ty (length exps)
                                    EQ ->     Right typ
                        else Left [(NotArray ty)]
                    Left errInt -> Left errInt
            Nothing -> Left [(Undefined name)]

    -- si es simplemente una referencia a una variable, devuelve su tipo
    | otherwise = case (getTypeByName name env) of
                    Just ty -> Right ty
                    Nothing -> Left [(Undefined name)]


{-================ CHEQUEO DE LITERALES ===============-}
getExpressionType (IntLit int) env = Right TyInt
getExpressionType (BoolLit bool) env = Right TyBool


{-================ CHEQUEO DE EXPRESIONES UNARIAS ===============-} 
getExpressionType (Unary uop expr) env
     -- asumiendo que neg debe aplicarse a expresiones booleanas unicamente
    | uop == Not = 
        case (getExpressionType expr env) of
            Right ty ->
                if (ty == TyBool)
                then Right TyBool
                else Left [(Expected TyBool ty)]
            Left errsExpr -> Left errsExpr

     -- asumiendo que neg debe aplicarse a expresiones enteras unicamente
    | uop == Neg =
        case (getExpressionType expr env) of
            Right ty ->
                if (ty == TyInt)
                then Right TyInt
                else Left [(Expected TyInt ty)]
            Left errsExpr -> Left errsExpr

            
{-================ CHEQUEO DE EXPRESIONES BINARIAS ===============-}            
getExpressionType (Binary bop exp1 exp2) env
    --ambas expresiones tienen que ser de tipo logico
    | bop == Or || bop == And =
        case (getExpressionType exp1 env) of
            Right ty1 ->
                case (getExpressionType exp2 env) of
                    Right ty2 ->
                        if (ty1 == TyBool)
                        then
                            if (ty2 == TyBool)
                            then Right TyBool
                            else Left [(Expected TyBool ty2)]
                        else
                            if (ty2 == TyBool)
                            then Left [(Expected TyBool ty1)]
                            else Left [(Expected TyBool ty1),(Expected TyBool ty2)]
                    Left errs2 -> Left errs2
            Left errs1 ->
                    if (isErrorUndefined (last errs1))
                    then  -- chequeo segunda expresion de la operacion binaria
                        case (getExpressionType exp2 env) of
                        Right ty2 -> Left errs1
                        Left errs2 -> Left (errs1 ++ errs2)
                    else 
                        -- chequeo segunda expresion de la operacion binaria
                        case (getExpressionType exp2 env) of
                        Right ty2 ->
                            if (ty2 == TyBool)
                            then Left errs1
                            else Left (errs1 ++ [(Expected TyInt ty2)])
                        Left errs2 -> Left (errs1 ++ errs2)

    -- las dos expresiones deben ser enteras
    | bop == Less || bop == Equ =
    -- pero se debe devolver TyBool, no como el caso de las operaciones
    -- enteras binarias
        -- chequeo primera expresion de la operacion binaria
        case (getExpressionType exp1 env) of
            Right ty1 ->
                -- chequeo segunda expresion de la operacion binaria
                case (getExpressionType exp2 env) of
                    Right ty2 ->
                        if (ty1 == TyInt)
                        then
                            if (ty2 == TyInt)
                            then Right TyBool
                            else Left [(Expected TyInt ty2)]
                        else
                            if (ty2 == TyInt)
                            then Left [(Expected TyInt ty1)]
                            else Left [(Expected TyInt ty1), (Expected TyInt ty2)]
                    Left errs2 -> Left errs2
            Left errs1 ->
                    if (isErrorUndefined (last errs1))
                    then  -- chequeo segunda expresion de la operacion binaria
                        case (getExpressionType exp2 env) of
                        Right ty2 -> Left errs1
                        Left errs2 -> Left (errs1 ++ errs2)
                    else 
                        -- chequeo segunda expresion de la operacion binaria
                        case (getExpressionType exp2 env) of
                        Right ty2 ->
                            if (ty2 == TyInt)
                            then Left errs1
                            else Left (errs1 ++ [(Expected TyInt ty2)])
                        Left errs2 -> Left (errs1 ++ errs2)
                    
   | otherwise = -- operaciones con enteros
        -- chequeo primera expresion de la operacion binaria
        case (getExpressionType exp1 env) of
            Right ty1 ->
                -- chequeo segunda expresion de la operacion binaria
                case (getExpressionType exp2 env) of
                    Right ty2 ->
                        if (ty1 == TyInt)
                        then
                            if (ty2 == TyInt)
                            then Right TyInt
                            else Left [(Expected TyInt ty2)]
                        else
                            if (ty2 == TyInt)
                            then Left [(Expected TyInt ty1)]
                            else Left [(Expected TyInt ty1), (Expected TyInt ty2)]
                    Left errs2 -> Left errs2
            Left errs1 ->
                    if (isErrorUndefined (last errs1))
                    then -- chequeo segunda expresion de la operacion binaria
                        case (getExpressionType exp2 env) of
                        Right ty2 -> Left errs1
                        Left errs2 -> Left (errs1 ++ errs2)
                    else 
                        -- chequeo segunda expresion de la operacion binaria
                        case (getExpressionType exp2 env) of
                        Right ty2 ->
                            if (ty2 == TyInt)
                            then Left errs1
                            else Left (errs1 ++ [(Expected TyInt ty2)])
                        Left errs2 -> Left (errs1 ++ errs2)


-- true si esta definida
checkVarDefined :: String -> Env -> Bool
checkVarDefined name env = length([name_temp | (name_temp, type_temp) <- env, name_temp == name]) > 0


-- devuelve la dimensión del tipo pasado 
-- x[2] -> 1, x[1][2] -> 2, x -> 0
checkArrayTypeDimensions :: Type -> Int
checkArrayTypeDimensions(TyArray ini fin ty) = (checkArrayTypeDimensions ty) + 1
checkArrayTypeDimensions(TyInt) = 0
checkArrayTypeDimensions(TyBool) = 0


-- devuelve True si el error es de tipo Undefined x, false en otro caso
isErrorUndefined :: Error -> Bool
isErrorUndefined (Undefined x) = True 
isErrorUndefined err = False

isArray :: Type -> Bool
isArray (TyArray ini fin ty) = True
isArray tipo = False
