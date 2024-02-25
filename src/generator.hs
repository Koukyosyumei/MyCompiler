module Generator where

import Parser

data Code = LCODE String | ERROR String deriving(Eq, Show)

codeGen :: [(String, [String])] -> [(String, Float)] -> ExprAST -> Code

codeGen _ _ (NumberExprAST val) = LCODE ("float " ++ (show val))

codeGen _ namedValue (VariableExprAST name) = 
    case (lookup name namedValue) of
        Just val -> LCODE ("float " ++ (show val))
        Nothing -> ERROR "Unknown variable name"
        
codeGen funcTable namedValue (BinaryExprAST op lhs rhs) =
    case op of
        '+' -> fADD lcode rcode "%addtmp"
        '-' -> fSUB lcode rcode "%subtmp"
        '*' -> fMUL lcode rcode "%multmp"
        '<' -> fCmpULT lcode rcode "%cmptmp"
        _ -> ERROR "invalid binary operator"
    where
        lcode = codeGen funcTable namedValue lhs
        rcode = codeGen funcTable namedValue rhs

codeGen funcTable namedValue (CallExprAST fname argExprs) = 
    case (lookup fname funcTable) of
        Just argNames -> if (length argNames /= length argExprs)
                            then ERROR "Incorrect # arguments passed"
                            else createCall fname (map (\arg -> codeGen funcTable namedValue arg) argExprs) "calltmp"  
        Nothing -> ERROR "Unknown functino referenced"    

codeGen funcTable namedValue (PrototypeAST fname argNames) = 
    LCODE ("declare float " ++ ("@" ++ fname) ++ ("(" ++ (joinWithCommaStr argNames) ++ ")"))

codeGen funcTable namedValue (FunctionAST prototype body) =
    case prototypeCODE of
        LCODE p -> case bodyCODE of
                    LCODE b -> LCODE (p ++ " {\n"
                                        ++ "entry:\n"
                                        ++ "\t" ++ b 
                                        ++ "}")
                    ERROR msg -> ERROR ("function body contains the following errors: " ++ msg)
        ERROR msg -> ERROR ("function declaration contains the following errors: " ++ msg)
    where
        prototypeCODE = codeGen funcTable namedValue prototype
        bodyCODE = codeGen funcTable namedValue body

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
joinWithComma [] = ""
joinWithComma [LCODE x] = x
joinWithComma ((LCODE x):xs) = x ++ ", " ++ joinWithComma xs
    
joinWithCommaStr :: [String] -> String
joinWithCommaStr [] = ""
joinWithCommaStr [x] = x
joinWithCommaStr (x:xs) = x ++ ", " ++ joinWithCommaStr xs
