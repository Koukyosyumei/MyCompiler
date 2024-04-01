module Generator where

import Parser

-- Data type representing the generated code
data Code
  = LCODE String
  | ERROR String
  deriving (Eq, Show)

-- Data type representing a symbolic value
data SInt
  = ACT Int
  | SYM String
  | PTR String
  deriving (Eq, Show)

-- Type alias for function environment (function name -> argument names)
type FEnv = [(String, [String])]

-- Type alias for variable environment (variable name -> symbolic value)
type VEnv = [(String, SInt)]

-- Function to extract code from a combined result (code, function env, variable env)
_getCode :: (Code, FEnv, VEnv) -> Code
_getCode (a, b, c) = a

-- Function to extract function environment from a combined result
_getFEnv :: (Code, FEnv, VEnv) -> FEnv
_getFEnv (a, b, c) = b

-- Function to extract variable environment from a combined result
_getVEnv :: (Code, FEnv, VEnv) -> VEnv
_getVEnv (a, b, c) = c

-- Function to get the actual value from a symbolic representation (variable name or pointer)
_getVal :: SInt -> VEnv -> (String, String)
_getVal (ACT x) _ = ("", show x)
_getVal (SYM x) _ = ("", x)
_getVal (PTR x) venv =
  ("\t" ++ v ++ " = load i32, i32* " ++ x ++ ", align 4", v)
  where
    v = generateNewVarName x venv -- Generate a new variable name to save the loaded value

-- Function to add a new variable name to the variable environment
addVarName :: [String] -> VEnv -> VEnv
addVarName [] venv = venv
addVarName (x:xs) venv = addVarName xs (venv ++ [(x, SYM ("%" ++ x))])

-- Function to check if an intermediate variable is needed for an expression (e.g., binary operations, function calls)
needIntermediateVar :: ExprAST -> VEnv -> Bool
needIntermediateVar (BinaryExprAST _ _ _) _ = True
needIntermediateVar (CallExprAST _ _) _ = True
needIntermediateVar (VariableExprAST x) venv =
  case lookup x venv of
    Just (PTR _) -> True
    _ -> False
needIntermediateVar _ _ = False

-- Function to check if the expression is a binary expression
isBinaryExpr :: ExprAST -> Bool
isBinaryExpr (BinaryExprAST _ _ _) = True
isBinaryExpr _ = False

-- Main function for code generation
codeGens :: FEnv -> VEnv -> [ExprAST] -> (Code, FEnv, VEnv)
codeGens fenv venv es = codeGens_ fenv venv es (LCODE "")

-- Helper function for recursive code generation
codeGens_ :: FEnv -> VEnv -> [ExprAST] -> Code -> (Code, FEnv, VEnv)
codeGens_ fenv venv [] code = (code, fenv, venv)
codeGens_ fenv venv (e:es) code =
  codeGens_ (_getFEnv e_result) (_getVEnv e_result) es new_code
  where
    e_result = codeGen fenv venv e
    new_code = codeAppend code (_getCode e_result)

-- Function to generate code for a single expression
codeGen :: FEnv -> VEnv -> ExprAST -> (Code, FEnv, VEnv)
codeGen fenv venv (NumberExprAST val) = (LCODE (show val), fenv, venv)
codeGen fenv venv (VariableExprAST name) =
  case (lookup name venv) of
    Just (ACT val) -> (LCODE ("i32 " ++ (show val)), fenv, venv)
    Just (SYM sym) -> (LCODE sym, fenv, venv)
    Just (PTR ptl) -> (LCODE ptl, fenv, venv)
    Nothing ->
      (ERROR ("Unknown variable name: name=" ++ name ++ "\n"), fenv, venv)
codeGen fenv venv (BinaryExprAST op lhs rhs) =
  fst (codeGenBinaryExpr fenv venv (BinaryExprAST op lhs rhs))
codeGen fenv venv (CallExprAST fname argExprs) =
  case (lookup fname fenv) of
    Just argNames ->
      if (length argNames /= length argExprs)
        then (ERROR "Incorrect # arguments passed", fenv, venv)
        else ( codeAppendN
                 intermediateCode
                 (createCall fname evaluatedArgs resultVar)
             , new_fenv
             , new_venv ++ [(resultVar, SYM resultVar)])
    Nothing -> (ERROR "Unknown functino referenced", fenv, venv)
  where
    resultVar = generateNewVarName "%calltmp" venv
    (new_fenv, new_venv, intermediateCode, evaluatedArgs) =
      createArgs fenv venv argExprs
codeGen fenv venv (PrototypeAST fname argNames) =
  ( LCODE
      ("define i32 "
         ++ ("@" ++ fname')
         ++ ("(" ++ (joinWithCommaStr argNames) ++ ")"))
  , fenv ++ [(fname', argNames)]
  , addVarName argNames venv)
  where
    fname' =
      if fname == ""
        then "0"
        else fname
codeGen fenv venv (FunctionAST prototype body) =
  case _getCode prototypeCODE of
    LCODE p ->
      case _getCode bodyCODE of
        LCODE b ->
          if (length localVars) == 0
            then (ERROR "A function should return a value.", newfenv, localVars)
            else ( LCODE
                     (p
                        ++ " {\n"
                        ++ "entry:\n"
                        ++ b
                        ++ (fst retInstr)
                        ++ "\n\tret i32 "
                        ++ (snd retInstr)
                        ++ "\n}\n")
                 , newfenv
                 , localVars)
        ERROR msg ->
          ( ERROR ("function body contains the following errors: " ++ msg)
          , fenv
          , venv)
    ERROR msg ->
      ( ERROR ("function declaration contains the following errors: " ++ msg)
      , fenv
      , venv)
  where
    prototypeCODE = codeGen fenv venv prototype
    newfenv = _getFEnv prototypeCODE
    bodyCODE = codeGen newfenv (venv ++ (_getVEnv prototypeCODE)) body
    localVars = _getVEnv bodyCODE
    retInstr_ = _getVal ((snd . last) localVars) venv
    retInstr =
      case (fst retInstr_) of
        "" -> ("", snd retInstr_)
        _ -> ("\n" ++ (fst retInstr_), snd retInstr_)
codeGen fenv venv (IfExprAST condAST thenAST elseAST) =
  (code, fenv, (_getVEnv elseBranch) ++ [("%iftmp", SYM "%iftmp")])
  where
    condCode = codeGen fenv venv condAST
    branchCode =
      LCODE
        ("\n\tbr i1 "
           ++ (fst . last . _getVEnv) condCode
           ++ ", label %then, label %else\n")
    entryCode = codeAppend (_getCode condCode) branchCode
    thenBranch = codeGen (_getFEnv condCode) (_getVEnv condCode) thenAST
    thenBody =
      case thenAST of
        NumberExprAST x -> LCODE ("")
        _ -> codeAppend (LCODE "\n") (_getCode thenBranch)
    thenRes =
      case thenAST of
        NumberExprAST x -> LCODE (show x)
        _ -> LCODE ((fst . last . _getVEnv) thenBranch)
    thenCode =
      codeAppends
        [LCODE ("\nthen:"), thenBody, LCODE ("\n\tbr label %ifcont\n")]
    elseBranch = codeGen (_getFEnv thenBranch) (_getVEnv thenBranch) elseAST
    elseBody =
      case elseAST of
        NumberExprAST x -> LCODE ("")
        _ -> codeAppend (LCODE "\n") (_getCode elseBranch)
    elseRes =
      case elseAST of
        NumberExprAST x -> LCODE (show x)
        _ -> LCODE ((fst . last . _getVEnv) elseBranch)
    elseCode =
      codeAppends
        [LCODE ("\nelse:"), elseBody, LCODE ("\n\tbr label %ifcont\n")]
    contCode =
      codeAppends
        [ LCODE ("\nifcont:\n\t%iftmp = phi i32 [ ")
        , thenRes
        , LCODE (", %then ], [ ")
        , elseRes
        , LCODE (", %else ]")
        ]
    code = codeAppends [entryCode, thenCode, elseCode, contCode]
codeGen fenv venv (BlockAST []) = (LCODE "", fenv, venv)
codeGen fenv venv (BlockAST (e:exprs)) =
  ( codeAppendN (_getCode e_result) (_getCode exprs_result)
  , _getFEnv exprs_result
  , _getVEnv exprs_result)
  where
    e_result = codeGen fenv venv (e)
    exprs_result =
      codeGen (_getFEnv e_result) (_getVEnv e_result) (BlockAST exprs)
codeGen fenv venv ast = (errormsg, fenv, venv)
  where
    errormsg =
      ERROR
        ("Unexpected inputs for codeGen:\n"
           ++ "\tfenv = "
           ++ (show fenv)
           ++ "\n\tvenv = "
           ++ (show venv)
           ++ "\n\tast = "
           ++ (show ast)
           ++ "\n")

generateNewVarName :: String -> VEnv -> String
generateNewVarName w venv = generateNewVarName' w 0 venv

generateNewVarName' :: String -> Int -> VEnv -> String
generateNewVarName' w i venv =
  case (lookup (w ++ show i) venv) of
    Just _ -> generateNewVarName' w (i + 1) venv
    Nothing -> w ++ show i

fADD :: Code -> Code -> String -> Code
fADD (LCODE op1) (LCODE op2) result =
  LCODE ("\t" ++ result ++ " = add i32 " ++ op1 ++ ", " ++ op2)
fADD (ERROR msg) _ _ = ERROR msg
fADD _ (ERROR msg) _ = ERROR msg

fSUB :: Code -> Code -> String -> Code
fSUB (LCODE op1) (LCODE op2) result =
  LCODE ("\t" ++ result ++ " = sub i32 " ++ op1 ++ ", " ++ op2)
fSUB (ERROR msg) _ _ = ERROR msg
fSUB _ (ERROR msg) _ = ERROR msg

fMUL :: Code -> Code -> String -> Code
fMUL (LCODE op1) (LCODE op2) result =
  LCODE ("\t" ++ result ++ " = mul i32 " ++ op1 ++ ", " ++ op2)
fMUL (ERROR msg) _ _ = ERROR msg
fMUL _ (ERROR msg) _ = ERROR msg

fCmpULT :: Code -> Code -> String -> Code
fCmpULT (LCODE op1) (LCODE op2) result =
  LCODE ("\t" ++ result ++ " = icmp ult i32 " ++ op1 ++ ", " ++ op2)
fCmpULT (ERROR msg) _ _ = ERROR msg
fCmpULT _ (ERROR msg) _ = ERROR msg

createCall :: String -> [Code] -> String -> Code
createCall fname args result =
  LCODE
    ("\t"
       ++ result
       ++ " = call i32 @"
       ++ fname
       ++ "("
       ++ (joinWithComma args)
       ++ ")")

codeAppend :: Code -> Code -> Code
codeAppend (ERROR msg) _ = ERROR msg
codeAppend _ (ERROR msg) = ERROR msg
codeAppend (LCODE a) (LCODE b) = LCODE (a ++ b)

codeAppends :: [Code] -> Code
codeAppends [] = LCODE ""
codeAppends (x:xs) = codeAppend x (codeAppends xs)

codeAppendN :: Code -> Code -> Code
codeAppendN (ERROR msg) _ = ERROR msg
codeAppendN _ (ERROR msg) = ERROR msg
codeAppendN (LCODE "") b = b
codeAppendN a (LCODE "") = a
codeAppendN (LCODE a) (LCODE b) = LCODE (a ++ "\n" ++ b)

createArgs :: FEnv -> VEnv -> [ExprAST] -> (FEnv, VEnv, Code, [Code])
createArgs ft nv [] = (ft, nv, LCODE "", [])
createArgs ft nv (a:aexprs) =
  ( n_ft
  , n_nv
  , codeAppendN a_intermediateCode n_intermediateCode
  , [a_evaluated] ++ n_evaluated)
  where
    a_intermediate = codeGen ft nv a
    (a_intermediateCode, a_evaluated) =
      case a of
        NumberExprAST num -> (LCODE "", LCODE (show num))
        VariableExprAST vname ->
          case lookup vname nv of
            Just (PTR ptr) -> (LCODE (fst load_ptr), LCODE (snd load_ptr))
              where load_ptr = _getVal (PTR ptr) nv
            _ -> (LCODE "", _getCode (codeGen ft nv a))
        _ ->
          ( _getCode (a_intermediate)
          , LCODE (fst (last (_getVEnv a_intermediate))))
    (n_ft, n_nv, n_intermediateCode, n_evaluated) =
      createArgs (_getFEnv a_intermediate) (_getVEnv a_intermediate) aexprs

codePrepStoreExpr :: FEnv -> VEnv -> ExprAST -> ((Code, FEnv, VEnv), String)
codePrepStoreExpr fenv venv (BinaryExprAST '=' (VariableExprAST vname) rhs) =
  case lookup vname venv of
    Just _ -> ((LCODE "", fenv, venv), "%" ++ vname)
    Nothing ->
      ( ( LCODE ("\t%" ++ vname ++ " = alloca i32, align 4\n")
        , fenv
        , venv ++ [(vname, PTR ("%" ++ vname))])
      , "%" ++ vname)
codePrepStoreExpr fenv venv _ =
  ((ERROR "The left term of `=` should be a variable name.", fenv, venv), "")

codeGenBinaryExpr :: FEnv -> VEnv -> ExprAST -> ((Code, FEnv, VEnv), String)
codeGenBinaryExpr fenv venv (BinaryExprAST op lhs rhs) =
  case op of
    '+' ->
      ( ( codeAppend intermediateCode (fADD lterm rterm newVar)
        , fenv
        , venvLR ++ [(newVar, SYM newVar)])
      , newVar)
    '-' ->
      ( ( codeAppend intermediateCode (fSUB lterm rterm newVar)
        , fenv
        , venvLR ++ [(newVar, SYM newVar)])
      , newVar)
    '*' ->
      ( ( codeAppend intermediateCode (fMUL lterm rterm newVar)
        , fenv
        , venvLR ++ [(newVar, SYM newVar)])
      , newVar)
    '<' ->
      ( ( codeAppend intermediateCode (fCmpULT lterm rterm newVar)
        , fenv
        , venvLR ++ [(newVar, SYM newVar)])
      , newVar)
    '=' ->
      ( ( codeAppends
            [ (_getCode (fst prepStoreExpr))
            , LCODE "\tstore i32 "
            , rterm
            , LCODE (", i32* " ++ (snd prepStoreExpr) ++ ", align 4")
            ]
        , fenv
        , _getVEnv (fst prepStoreExpr))
      , snd prepStoreExpr)
    _ -> ((ERROR "invalid binary operator", fenv, venvLR), newVar)
  where
    lbranch = codeGenBinaryExpr fenv venv lhs
    intermediateCodeL =
      if needIntermediateVar lhs venv
        then codeAppend (_getCode (fst lbranch)) (LCODE "\n")
        else LCODE ("")
    lterm =
      if needIntermediateVar lhs venv
        then LCODE (snd lbranch)
        else _getCode (fst lbranch)
    venvL =
      if needIntermediateVar lhs venv
        then venv ++ (_getVEnv (fst lbranch))
        else venv
    rbranch = codeGenBinaryExpr fenv venvL rhs
    intermediateCodeR =
      if needIntermediateVar rhs venv
        then codeAppend (_getCode (fst rbranch)) (LCODE "\n")
        else LCODE ("")
    rterm =
      if needIntermediateVar rhs venv
        then LCODE (snd rbranch)
        else _getCode (fst rbranch)
    venvLR =
      if needIntermediateVar rhs venv
        then venvL ++ (_getVEnv (fst rbranch))
        else venvL
    newVar =
      case op of
        '+' -> generateNewVarName "%addtmp" venvLR
        '-' -> generateNewVarName "%subtmp" venvLR
        '*' -> generateNewVarName "%multmp" venvLR
        '<' -> generateNewVarName "%cmptmp" venvLR
    intermediateCode = codeAppend intermediateCodeL intermediateCodeR
    prepStoreExpr = codePrepStoreExpr fenv venv (BinaryExprAST op lhs rhs)
codeGenBinaryExpr fenv venv (CallExprAST fname argExprs) =
  (callResult, fst (last (_getVEnv callResult)))
  where
    callResult = codeGen fenv venv (CallExprAST fname argExprs)
codeGenBinaryExpr fenv venv (VariableExprAST x) =
  case lookup x venv of
    Just (PTR ptr) ->
      ( ( LCODE ("\t" ++ v ++ " = load i32, i32* " ++ ptr ++ ", align 4")
        , fenv
        , venv ++ [(v, SYM v)])
      , v)
      where v = generateNewVarName ptr venv
    _ -> (codeGen fenv venv (VariableExprAST x), "")
codeGenBinaryExpr fenv venv exp = (codeGen fenv venv exp, "")

joinWithComma :: [Code] -> String
joinWithComma [] = ""
joinWithComma [LCODE x] = "i32 " ++ x
joinWithComma ((LCODE x):xs) = "i32 " ++ x ++ ", " ++ joinWithComma xs

joinWithCommaStr :: [String] -> String
joinWithCommaStr [] = ""
joinWithCommaStr [x] = "i32 %" ++ x
joinWithCommaStr (x:xs) = "i32 %" ++ x ++ ", " ++ joinWithCommaStr xs
