module Generator where

import           Parser

data Code = LCODE String | ERROR String deriving(Eq, Show)
data SInt = ACT Int | SYM String deriving(Eq, Show)

type FEnv = [(String, [String])]
type VEnv = [(String, SInt)]

_getCode :: (Code, FEnv, VEnv) -> Code
_getCode (a, b, c) = a

_getFEnv :: (Code, FEnv, VEnv) -> FEnv
_getFEnv (a, b, c) = b

_getVEnv :: (Code, FEnv, VEnv) -> VEnv
_getVEnv (a, b, c) = c

_getVal :: SInt -> String
_getVal (ACT x) = show x
_getVal (SYM x) = x

addVarName :: [String] -> VEnv -> VEnv
addVarName [] venv     = venv
addVarName (x:xs) venv = addVarName xs (venv ++ [(x, SYM ("%" ++ x))])

needIntermediateVar :: ExprAST -> Bool
needIntermediateVar (BinaryExprAST _ _ _) = True
needIntermediateVar (CallExprAST _ _)     = True
needIntermediateVar _                     = False

isBinaryExpr :: ExprAST -> Bool
isBinaryExpr (BinaryExprAST _ _ _) = True
isBinaryExpr _                     = False

codeGens :: FEnv -> VEnv -> [ExprAST] -> (Code, FEnv, VEnv)
codeGens fenv venv es = codeGens_ fenv venv es (LCODE "")

codeGens_ :: FEnv -> VEnv -> [ExprAST] -> Code -> (Code, FEnv, VEnv)
codeGens_ fenv venv     [] code = (code, fenv, venv)
codeGens_ fenv venv (e:es) code = codeGens_ (_getFEnv e_result) (_getVEnv e_result) es new_code
    where
        e_result = codeGen fenv venv e
        new_code = codeAppend code (_getCode e_result)

codeGen :: FEnv -> VEnv -> ExprAST -> (Code, FEnv, VEnv)

codeGen fenv venv (NumberExprAST val) = (LCODE (show val), fenv, venv)

codeGen fenv namedValue (VariableExprAST name) =
    case (lookup name namedValue) of
        Just (ACT val) -> (LCODE ("i32 " ++ (show val)), fenv, namedValue)
        Just (SYM sym) -> (LCODE sym, fenv, namedValue)
        Nothing        -> (ERROR ("Unknown variable name: name=" ++ name ++ "\n"), fenv, namedValue)

codeGen funcTable namedValue (BinaryExprAST op lhs rhs) = fst (codeGenBinaryExpr funcTable namedValue (BinaryExprAST op lhs rhs))

codeGen funcTable namedValue (CallExprAST fname argExprs) =
    case (lookup fname funcTable) of
        Just argNames -> if (length argNames /= length argExprs)
                            then (ERROR "Incorrect # arguments passed", funcTable, namedValue)
                            else (codeAppendN intermediateCode (createCall fname evaluatedArgs resultVar),
                                 new_funcTable, new_namedValue ++ [(resultVar, SYM resultVar)])
        Nothing       -> (ERROR "Unknown functino referenced", funcTable, namedValue)
    where
        resultVar = generateNewVarName "%calltmp" namedValue
        (new_funcTable, new_namedValue, intermediateCode, evaluatedArgs) = createArgs funcTable namedValue argExprs

codeGen funcTable namedValue (PrototypeAST fname argNames) =
    (LCODE ("define i32 " ++ ("@" ++ fname') ++ ("(" ++ (joinWithCommaStr argNames) ++ ")")),
     funcTable ++ [(fname', argNames)],
     addVarName argNames namedValue)
    where
        fname' = if fname == "" then "0" else fname

codeGen funcTable namedValue (FunctionAST prototype body) =
    case _getCode prototypeCODE of
        LCODE p -> case _getCode bodyCODE of
                    LCODE b   -> if (length localVars) == 0
                                    then (ERROR "A function should return a value.", newfuncTable, localVars)
                                    else (LCODE (p ++ " {\n"
                                        ++ "entry:\n"
                                        ++ b
                                        ++ "\n\tret i32 " ++ (_getVal . snd . last) localVars
                                        ++ "\n}\n"),
                                        newfuncTable,
                                        localVars)
                    ERROR msg -> (ERROR ("function body contains the following errors: " ++ msg), funcTable, namedValue)
        ERROR msg -> (ERROR ("function declaration contains the following errors: " ++ msg), funcTable, namedValue)
    where
        prototypeCODE = codeGen funcTable namedValue prototype
        newfuncTable  = _getFEnv prototypeCODE
        bodyCODE      = codeGen newfuncTable (namedValue ++ (_getVEnv prototypeCODE)) body
        localVars     = _getVEnv bodyCODE

codeGen funcTable namedValue (IfExprAST condAST thenAST elseAST) = (code, funcTable,
                                                                    (_getVEnv elseBranch) ++ [("%iftmp", SYM "%iftmp")])
    where
        condCode   = codeGen funcTable namedValue condAST
        branchCode = LCODE ("\n\tbr i1 " ++ (fst . last. _getVEnv) condCode ++ ", label %then, label %else\n")
        entryCode  = codeAppend (_getCode condCode) branchCode

        thenBranch = codeGen (_getFEnv condCode) (_getVEnv condCode) thenAST
        thenBody   = case thenAST of
                     NumberExprAST x -> LCODE ("")
                     _               -> codeAppend (LCODE "\n") (_getCode thenBranch)
        thenRes    = case thenAST of
                     NumberExprAST x -> LCODE (show x)
                     _               -> LCODE ((fst . last . _getVEnv) thenBranch)
        thenCode   = codeAppends [LCODE ("\nthen:"), thenBody, LCODE ("\n\tbr label %ifcont\n")]

        elseBranch = codeGen (_getFEnv thenBranch) (_getVEnv thenBranch) elseAST
        elseBody   = case elseAST of
                     NumberExprAST x -> LCODE ("")
                     _               -> codeAppend (LCODE "\n") (_getCode elseBranch)
        elseRes    = case elseAST of
                     NumberExprAST x -> LCODE (show x)
                     _               -> LCODE ((fst . last . _getVEnv) elseBranch)
        elseCode   = codeAppends [LCODE ("\nelse:"), elseBody, LCODE ("\n\tbr label %ifcont\n")]

        contCode   = codeAppends [LCODE ("\nifcont:\n\t%iftmp = phi i32 [ "),
                                thenRes, LCODE (", %then ], [ "), elseRes, LCODE (", %else ]")]

        code       = codeAppends [entryCode, thenCode, elseCode, contCode]

codeGen funcTable namedValue (BlockAST        []) = (LCODE "", funcTable, namedValue)
codeGen funcTable namedValue (BlockAST (e:exprs)) = (codeAppendN (_getCode e_result) (_getCode exprs_result), _getFEnv exprs_result, _getVEnv exprs_result)
    where
        e_result = codeGen funcTable namedValue (e)
        exprs_result = codeGen (_getFEnv e_result) (_getVEnv e_result) (BlockAST exprs)

codeGen funcTable namedValue ast = (errormsg, funcTable, namedValue)
    where
        errormsg = ERROR ("Unexpected inputs for codeGen:\n" ++ "\tfuncTable = " ++ (show funcTable) ++ "\n\tnamedValue = " ++ (show namedValue) ++ "\n\tast = " ++ (show ast) ++ "\n")

generateNewVarName :: String -> VEnv -> String
generateNewVarName w venv = generateNewVarName' w 0 venv

generateNewVarName' :: String -> Int -> VEnv -> String
generateNewVarName' w i venv =
    case (lookup (w ++ show i) venv) of
        Just _  -> generateNewVarName' w (i + 1) venv
        Nothing -> w ++ show i

fADD :: Code -> Code -> String -> Code
fADD (LCODE op1) (LCODE op2) result = LCODE ("\t" ++ result ++ " = add i32 " ++ op1 ++ ", " ++ op2)
fADD (ERROR msg) _ _ = ERROR msg
fADD _ (ERROR msg) _ = ERROR msg

fSUB :: Code -> Code -> String -> Code
fSUB (LCODE op1) (LCODE op2) result = LCODE ("\t" ++ result ++ " = sub i32 " ++ op1 ++ ", " ++ op2)
fSUB (ERROR msg) _ _ = ERROR msg
fSUB _ (ERROR msg) _ = ERROR msg

fMUL :: Code -> Code -> String -> Code
fMUL (LCODE op1) (LCODE op2) result = LCODE ("\t" ++ result ++ " = mul i32 " ++ op1 ++ ", " ++ op2)
fMUL (ERROR msg) _ _ = ERROR msg
fMUL _ (ERROR msg) _ = ERROR msg

fCmpULT :: Code -> Code -> String -> Code
fCmpULT (LCODE op1) (LCODE op2) result = LCODE ("\t" ++ result ++ " = icmp ult i32 " ++ op1 ++ ", " ++ op2)
fCmpULT (ERROR msg) _ _ = ERROR msg
fCmpULT _ (ERROR msg) _ = ERROR msg

createCall :: String -> [Code] -> String -> Code
createCall fname args result = LCODE ("\t" ++ result ++ " = call i32 @" ++ fname ++ "(" ++ (joinWithComma args) ++ ")")

codeAppend :: Code -> Code -> Code
codeAppend (ERROR msg) _       = ERROR msg
codeAppend _ (ERROR msg)       = ERROR msg
codeAppend (LCODE a) (LCODE b) = LCODE (a ++ b)

codeAppends :: [Code] -> Code
codeAppends []     = LCODE ""
codeAppends (x:xs) = codeAppend x (codeAppends xs)

codeAppendN :: Code -> Code -> Code
codeAppendN (ERROR msg) _       = ERROR msg
codeAppendN _ (ERROR msg)       = ERROR msg
codeAppendN (LCODE "") b        = b
codeAppendN a (LCODE "")        = a
codeAppendN (LCODE a) (LCODE b) = LCODE (a ++ "\n" ++ b)

createArgs :: FEnv -> VEnv -> [ExprAST] -> (FEnv, VEnv, Code, [Code])
createArgs ft nv [] = (ft, nv, LCODE "", [])
createArgs ft nv (a:aexprs) = (n_ft, n_nv, codeAppendN a_intermediateCode n_intermediateCode, [a_evaluated] ++ n_evaluated)
    where
        a_intermediate = codeGen ft nv a
        (a_intermediateCode, a_evaluated) = case a of
            NumberExprAST num -> (LCODE "", LCODE (show num))
            VariableExprAST vname -> (LCODE "",  _getCode (codeGen ft nv a))
            _ -> (_getCode (a_intermediate), LCODE (fst (last (_getVEnv a_intermediate))))
        (n_ft, n_nv, n_intermediateCode, n_evaluated) = createArgs (_getFEnv a_intermediate) (_getVEnv a_intermediate) aexprs

codePrepStoreExpr:: FEnv -> VEnv -> ExprAST -> ((Code, FEnv, VEnv), String)
codePrepStoreExpr funcTable namedValue (BinaryExprAST '=' (VariableExprAST vname) rhs) =
    case lookup vname namedValue of
        Just _ -> ((LCODE "", funcTable, namedValue), "%" ++ vname)
        Nothing -> ((LCODE ("\t%" ++ vname ++ " = alloca i32, align 4\n"), funcTable, namedValue ++ [(vname, SYM ("%" ++ vname))]), "%" ++ vname)
codePrepStoreExpr funcTable namedValue _ = ((ERROR "The left term of `=` should be a variable name.", funcTable, namedValue), "")


codeGenBinaryExpr:: FEnv -> VEnv -> ExprAST -> ((Code, FEnv, VEnv), String)
codeGenBinaryExpr funcTable namedValue (BinaryExprAST op lhs rhs) =
    case op of
        '+' -> ((codeAppend intermediateCode (fADD lterm rterm newVar), funcTable, namedValueLR ++ [(newVar, SYM newVar)]), newVar)
        '-' -> ((codeAppend intermediateCode (fSUB lterm rterm newVar), funcTable, namedValueLR ++ [(newVar, SYM newVar)]), newVar)
        '*' -> ((codeAppend intermediateCode (fMUL lterm rterm newVar), funcTable, namedValueLR ++ [(newVar, SYM newVar)]), newVar)
        '<' -> ((codeAppend intermediateCode (fCmpULT lterm rterm newVar), funcTable, namedValueLR ++ [(newVar, SYM newVar)]), newVar)
        '=' -> ((codeAppends [(_getCode (fst prepStoreExpr)),
                               LCODE "\tstore i32 ", rterm,
                               LCODE (", i32* " ++ (snd prepStoreExpr) ++ ", align 4")], funcTable, _getVEnv (fst prepStoreExpr)),
                               snd prepStoreExpr)
        _ -> ((ERROR "invalid binary operator", funcTable, namedValueLR), newVar)
    where
        lbranch = codeGenBinaryExpr funcTable namedValue lhs
        intermediateCodeL = if needIntermediateVar lhs then codeAppend (_getCode (fst lbranch)) (LCODE "\n") else LCODE ("")
        lterm = if needIntermediateVar lhs then LCODE (snd lbranch) else _getCode (fst lbranch)
        namedValueL = if needIntermediateVar lhs then namedValue ++ (_getVEnv (fst lbranch)) else namedValue

        rbranch = codeGenBinaryExpr funcTable namedValueL rhs
        intermediateCodeR = if needIntermediateVar rhs then codeAppend (_getCode (fst rbranch)) (LCODE "\n") else LCODE ("")
        rterm = if needIntermediateVar rhs then LCODE (snd rbranch) else _getCode (fst rbranch)
        namedValueLR = if needIntermediateVar rhs then  namedValueL ++ (_getVEnv (fst rbranch)) else namedValueL

        newVar = case op of
                        '+' -> generateNewVarName "%addtmp" namedValueLR
                        '-' -> generateNewVarName "%subtmp" namedValueLR
                        '*' -> generateNewVarName "%multmp" namedValueLR
                        '<' -> generateNewVarName "%cmptmp" namedValueLR
        intermediateCode = codeAppend intermediateCodeL intermediateCodeR

        prepStoreExpr = codePrepStoreExpr funcTable namedValue (BinaryExprAST op lhs rhs)

codeGenBinaryExpr funcTable namedValue (CallExprAST fname argExprs) = (callResult, fst (last (_getVEnv callResult)))
    where
        callResult = codeGen funcTable namedValue (CallExprAST fname argExprs)

codeGenBinaryExpr funcTable namedValue exp = (codeGen funcTable namedValue exp, "")

joinWithComma :: [Code] -> String
joinWithComma []             = ""
joinWithComma [LCODE x]      = "i32 " ++ x
joinWithComma ((LCODE x):xs) = "i32 " ++ x ++ ", " ++ joinWithComma xs

joinWithCommaStr :: [String] -> String
joinWithCommaStr []     = ""
joinWithCommaStr [x]    = "i32 %" ++ x
joinWithCommaStr (x:xs) = "i32 %" ++ x ++ ", " ++ joinWithCommaStr xs
