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
addVarName (x:xs) venv = venv ++ [(x, SYM x)]

codeGen :: FEnv -> VEnv -> ExprAST -> (Code, FEnv, VEnv)

codeGen fenv venv (NumberExprAST val) = (LCODE (show val), fenv, venv)

codeGen fenv namedValue (VariableExprAST name) =
    case (lookup name namedValue) of
        Just (ACT val) -> (LCODE ("float " ++ (show val)), fenv, namedValue)
        Just (SYM sym) -> (LCODE sym, fenv, namedValue)
        Nothing        -> (ERROR "Unknown variable name", fenv, namedValue)

codeGen funcTable namedValue (BinaryExprAST op lhs rhs) =
    case op of
        '+' -> (fADD (_getCode lcode) (_getCode rcode) newVar, funcTable, namedValue ++ [(newVar, SYM newVar)])
        '-' -> (fSUB (_getCode lcode) (_getCode rcode) newVar, funcTable, namedValue ++ [(newVar, SYM newVar)])
        '*' -> (fMUL (_getCode lcode) (_getCode rcode) newVar, funcTable, namedValue ++ [(newVar, SYM newVar)])
        '<' -> (fCmpULT (_getCode lcode) (_getCode rcode) newVar, funcTable, namedValue)
        _ -> (ERROR "invalid binary operator", funcTable, namedValue)
    where
        lcode = codeGen funcTable namedValue lhs
        rcode = codeGen funcTable namedValue rhs
        newVar = case op of
                        '+' -> generateNewVarName "%addtmp" namedValue
                        '-' -> generateNewVarName "%subtmp" namedValue
                        '*' -> generateNewVarName "%multmp" namedValue
                        '<' -> generateNewVarName "%cmptmp" namedValue

codeGen funcTable namedValue (CallExprAST fname argExprs) =
    case (lookup fname funcTable) of
        Just argNames -> if (length argNames /= length argExprs)
                            then (ERROR "Incorrect # arguments passed", funcTable, namedValue)
                            else (createCall fname (map (\arg -> _getCode (codeGen funcTable namedValue arg)) argExprs) "calltmp",
                                 funcTable, namedValue)
        Nothing -> (ERROR "Unknown functino referenced", funcTable, namedValue)

codeGen funcTable namedValue (PrototypeAST fname argNames) =
    (LCODE ("declare float " ++ ("@" ++ fname') ++ ("(" ++ (joinWithCommaStr argNames) ++ ")")), 
     funcTable, 
     addVarName argNames namedValue)
    where
        fname' = if fname == "" then "0" else fname

codeGen funcTable namedValue (FunctionAST prototype body) =
    case _getCode prototypeCODE of
        LCODE p -> case _getCode bodyCODE of
                    LCODE b -> (LCODE (p ++ " {\n"
                                        ++ "entry:\n"
                                        ++ "\t" ++ b
                                        ++ "\n}\n"), 
                                funcTable, 
                                namedValue)
                    ERROR msg -> (ERROR ("function body contains the following errors: " ++ msg), funcTable, namedValue)
        ERROR msg -> (ERROR ("function declaration contains the following errors: " ++ msg), funcTable, namedValue)
    where
        prototypeCODE = codeGen funcTable namedValue prototype
        bodyCODE = codeGen funcTable (namedValue ++ (_getVEnv prototypeCODE)) body

generateNewVarName :: String -> VEnv -> String
generateNewVarName w venv = generateNewVarName' w 0 venv

generateNewVarName' :: String -> Int -> VEnv -> String
generateNewVarName' w i venv =
    case (lookup (w ++ show i) venv) of
        Just _  -> generateNewVarName' w (i + 1) venv
        Nothing -> w ++ show i

fADD :: Code -> Code -> String -> Code
fADD (LCODE op1) (LCODE op2) result = LCODE (result ++ " = fadd float " ++ op1 ++ ", " ++ op2)
fADD (ERROR msg) _ _ = ERROR msg
fADD _ (ERROR msg) _ = ERROR msg

fSUB :: Code -> Code -> String -> Code
fSUB (LCODE op1) (LCODE op2) result = LCODE (result ++ " = fsub float " ++ op1 ++ ", " ++ op2)
fSUB (ERROR msg) _ _ = ERROR msg
fSUB _ (ERROR msg) _ = ERROR msg

fMUL :: Code -> Code -> String -> Code
fMUL (LCODE op1) (LCODE op2) result = LCODE (result ++ " = fmul float " ++ op1 ++ ", " ++ op2)
fMUL (ERROR msg) _ _ = ERROR msg
fMUL _ (ERROR msg) _ = ERROR msg

fCmpULT :: Code -> Code -> String -> Code
fCmpULT (LCODE op1) (LCODE op2) result = LCODE (result ++ " = fcmp ult float " ++ op1 ++ ", " ++ op2)
fCmpULT (ERROR msg) _ _ = ERROR msg
fCmpULT _ (ERROR msg) _ = ERROR msg

createCall :: String -> [Code] -> String -> Code
createCall fname args result = LCODE (result ++ " = call float @" ++ fname ++ "(" ++ (joinWithComma args) ++ ")")

joinWithComma :: [Code] -> String
joinWithComma []             = ""
joinWithComma [LCODE x]      = x
joinWithComma ((LCODE x):xs) = x ++ ", " ++ joinWithComma xs

joinWithCommaStr :: [String] -> String
joinWithCommaStr []     = ""
joinWithCommaStr [x]    = x
joinWithCommaStr (x:xs) = x ++ ", " ++ joinWithCommaStr xs
