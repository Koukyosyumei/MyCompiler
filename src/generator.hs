module Generator where

import           Parser

data Code = LCODE String | ERROR String deriving(Eq, Show)
data SFloat = ACT Float | SYM String deriving(Eq, Show)

type FEnv = [(String, [String])]
type VEnv = [(String, SFloat)]

_getCode :: (Code, FEnv, VEnv) -> Code
_getCode (a, b, c) = a

_getFEnv :: (Code, FEnv, VEnv) -> FEnv
_getFEnv (a, b, c) = b

_getVEnv :: (Code, FEnv, VEnv) -> VEnv
_getVEnv (a, b, c) = c

addVarName :: [String] -> VEnv -> VEnv
addVarName [] venv = venv
addVarName (x:xs) venv = addVarName xs (venv ++ [(x, SYM ("%" ++ x))])

needIntermediateVar :: ExprAST -> Bool
needIntermediateVar (BinaryExprAST _ _ _) = True
needIntermediateVar (CallExprAST _ _) = True
needIntermediateVar _ = False

isBinaryExpr :: ExprAST -> Bool
isBinaryExpr (BinaryExprAST _ _ _) = True
isBinaryExpr _ = False

codeGen :: FEnv -> VEnv -> ExprAST -> (Code, FEnv, VEnv)

codeGen fenv venv (NumberExprAST val) = (LCODE (show val), fenv, venv)

codeGen fenv namedValue (VariableExprAST name) =
    case (lookup name namedValue) of
        Just (ACT val) -> (LCODE ("double " ++ (show val)), fenv, namedValue)
        Just (SYM sym) -> (LCODE sym, fenv, namedValue)
        Nothing        -> (ERROR ("Unknown variable name: name=" ++ name ++ "\n"), fenv, namedValue)

codeGen funcTable namedValue (BinaryExprAST op lhs rhs) = fst (codeGenBinaryExpr funcTable namedValue (BinaryExprAST op lhs rhs))

codeGen funcTable namedValue (CallExprAST fname argExprs) =
    case (lookup fname funcTable) of
        Just argNames -> if (length argNames /= length argExprs)
                            then (ERROR "Incorrect # arguments passed", funcTable, namedValue)
                            else (createCall fname (map (\arg -> _getCode (codeGen funcTable namedValue arg)) argExprs) resultVar,
                                 funcTable, namedValue ++ [(resultVar, SYM resultVar)])
        Nothing -> (ERROR "Unknown functino referenced", funcTable, namedValue)
    where
        resultVar = generateNewVarName "%calltmp" namedValue

codeGen funcTable namedValue (PrototypeAST fname argNames) =
    (LCODE ("define double " ++ ("@" ++ fname') ++ ("(" ++ (joinWithCommaStr argNames) ++ ")")), 
     funcTable ++ [(fname', argNames)], 
     addVarName argNames namedValue)
    where
        fname' = if fname == "" then "0" else fname

codeGen funcTable namedValue (FunctionAST prototype body) =
    case _getCode prototypeCODE of
        LCODE p -> case _getCode bodyCODE of
                    LCODE b -> (LCODE (p ++ " {\n"
                                        ++ "entry:\n"
                                        ++ "\t" ++ b
                                        ++ "\n\tret double " ++ (fst (localVars !! ((length localVars) - 1)))
                                        ++ "\n}\n"), 
                                newfuncTable, 
                                localVars)
                    ERROR msg -> (ERROR ("function body contains the following errors: " ++ msg), funcTable, namedValue)
        ERROR msg -> (ERROR ("function declaration contains the following errors: " ++ msg), funcTable, namedValue)
    where
        prototypeCODE = codeGen funcTable namedValue prototype
        newfuncTable = _getFEnv prototypeCODE
        bodyCODE = codeGen newfuncTable (namedValue ++ (_getVEnv prototypeCODE)) body
        localVars = _getVEnv bodyCODE

codeGen funcTable namedValue (IfExprAST condAST thenAST elseAST) = (code, funcTable, (_getVEnv elseBranch) ++ [("%iftmp", SYM "%iftmp")])
    where
        condCode = codeGen funcTable namedValue condAST
        condRes  = LCODE ("\n\t%ifcond = " ++ fst (last (_getVEnv condCode)))
        branchCode = LCODE ("\n\tbr i1 %ifcond, label %then, label %else\n")
        entryCode = codeAppend (codeAppend (_getCode condCode) condRes) branchCode

        thenBranch = codeGen (_getFEnv condCode) (_getVEnv condCode) thenAST
        thenBody = case thenAST of
                     NumberExprAST x -> LCODE ("")
                     _ -> codeAppend (LCODE "\n\t") (_getCode thenBranch)
        thenRes = case thenAST of 
                     NumberExprAST x -> LCODE (show x)
                     _ -> LCODE (fst (last (_getVEnv thenBranch)))
        thenCode = codeAppend (codeAppend (LCODE ("\nthen:")) thenBody)
                              (LCODE ("\n\tbr label %ifcont\n"))

        elseBranch = codeGen (_getFEnv thenBranch) (_getVEnv thenBranch) elseAST
        elseBody = case elseAST of
                     NumberExprAST x -> LCODE ("")
                     _ -> codeAppend (LCODE "\n\t") (_getCode elseBranch)
        elseRes = case elseAST of
                     NumberExprAST x -> LCODE (show x)
                     _ -> LCODE (fst (last (_getVEnv elseBranch)))
        elseCode = codeAppend (codeAppend (LCODE ("\nelse:")) elseBody)
                              (LCODE ("\n\tbr label %ifcont\n"))

        contCode = codeAppend (codeAppend (LCODE ("\nifcont:\n\t%iftmp = phi double [ ")) thenRes)
                              (codeAppend (LCODE (", %then ], [ ")) (codeAppend elseRes (LCODE (", %else ]"))))

        code = codeAppend (codeAppend (codeAppend entryCode thenCode) elseCode) contCode

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
fADD (LCODE op1) (LCODE op2) result = LCODE (result ++ " = fadd double " ++ op1 ++ ", " ++ op2)
fADD (ERROR msg) _ _ = ERROR msg
fADD _ (ERROR msg) _ = ERROR msg

fSUB :: Code -> Code -> String -> Code
fSUB (LCODE op1) (LCODE op2) result = LCODE (result ++ " = fsub double " ++ op1 ++ ", " ++ op2)
fSUB (ERROR msg) _ _ = ERROR msg
fSUB _ (ERROR msg) _ = ERROR msg

fMUL :: Code -> Code -> String -> Code
fMUL (LCODE op1) (LCODE op2) result = LCODE (result ++ " = fmul double " ++ op1 ++ ", " ++ op2)
fMUL (ERROR msg) _ _ = ERROR msg
fMUL _ (ERROR msg) _ = ERROR msg

fCmpULT :: Code -> Code -> String -> Code
fCmpULT (LCODE op1) (LCODE op2) result = LCODE (result ++ " = fcmp ult double " ++ op1 ++ ", " ++ op2)
fCmpULT (ERROR msg) _ _ = ERROR msg
fCmpULT _ (ERROR msg) _ = ERROR msg

createCall :: String -> [Code] -> String -> Code
createCall fname args result = LCODE (result ++ " = call double @" ++ fname ++ "(" ++ (joinWithComma args) ++ ")")

codeAppend :: Code -> Code -> Code
codeAppend (LCODE a) (LCODE b) = LCODE (a ++ b)


codeGenBinaryExpr:: FEnv -> VEnv -> ExprAST -> ((Code, FEnv, VEnv), String)
codeGenBinaryExpr funcTable namedValue (BinaryExprAST op lhs rhs) =
    case op of
        '+' -> ((codeAppend intermediateCode (fADD lterm rterm newVar), funcTable, namedValueLR ++ [(newVar, SYM newVar)]), newVar)
        '-' -> ((codeAppend intermediateCode (fSUB lterm rterm newVar), funcTable, namedValueLR ++ [(newVar, SYM newVar)]), newVar)
        '*' -> ((codeAppend intermediateCode (fMUL lterm rterm newVar), funcTable, namedValueLR ++ [(newVar, SYM newVar)]), newVar)
        '<' -> ((codeAppend intermediateCode (fCmpULT lterm rterm newVar), funcTable, namedValueLR ++ [(newVar, SYM newVar)]), newVar)
        _ -> ((ERROR "invalid binary operator", funcTable, namedValueLR), newVar)
    where
        lbranch = codeGenBinaryExpr funcTable namedValue lhs
        intermediateCodeL = if needIntermediateVar lhs then codeAppend (_getCode (fst lbranch)) (LCODE "\n\t") else LCODE ("")
        lterm = if needIntermediateVar lhs then LCODE (snd lbranch) else _getCode (fst lbranch)
        namedValueL = if needIntermediateVar lhs then namedValue ++ (_getVEnv (fst lbranch)) else namedValue
        
        rbranch = codeGenBinaryExpr funcTable namedValueL rhs
        intermediateCodeR = if needIntermediateVar rhs then codeAppend (_getCode (fst rbranch)) (LCODE "\n\t") else LCODE ("")
        rterm = if needIntermediateVar rhs then LCODE (snd rbranch) else _getCode (fst rbranch)
        namedValueLR = if needIntermediateVar rhs then  namedValueL ++ (_getVEnv (fst rbranch)) else namedValueL

        newVar = case op of
                        '+' -> generateNewVarName "%addtmp" namedValueLR
                        '-' -> generateNewVarName "%subtmp" namedValueLR
                        '*' -> generateNewVarName "%multmp" namedValueLR
                        '<' -> generateNewVarName "%cmptmp" namedValueLR
        intermediateCode = codeAppend intermediateCodeL intermediateCodeR

codeGenBinaryExpr funcTable namedValue (CallExprAST fname argExprs) = (callResult, fst (last (_getVEnv callResult)))
    where
        callResult = codeGen funcTable namedValue (CallExprAST fname argExprs)

codeGenBinaryExpr funcTable namedValue exp = (codeGen funcTable namedValue exp, "")

joinWithComma :: [Code] -> String
joinWithComma []             = ""
joinWithComma [LCODE x]      = "double " ++ x
joinWithComma ((LCODE x):xs) = "double " ++ x ++ ", " ++ joinWithComma xs

joinWithCommaStr :: [String] -> String
joinWithCommaStr []     = ""
joinWithCommaStr [x]    = "%" ++ x
joinWithCommaStr (x:xs) = "%" ++ x ++ ", " ++ joinWithCommaStr xs
