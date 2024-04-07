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

-- Type alias for block environment (block type -> latest name)
type BEnv = [(String, SInt)]

-- Function to extract code from a combined result (code, function env, variable env, block env)
_getCode :: (Code, FEnv, VEnv, BEnv) -> Code
_getCode (a, b, c, d) = a

-- Function to extract function environment from a combined result
_getFEnv :: (Code, FEnv, VEnv, BEnv) -> FEnv
_getFEnv (a, b, c, d) = b

-- Function to extract variable environment from a combined result
_getVEnv :: (Code, FEnv, VEnv, BEnv) -> VEnv
_getVEnv (a, b, c, d) = c

_getBEnv :: (Code, FEnv, VEnv, BEnv) -> BEnv
_getBEnv (a, b, c, d) = d

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
codeGens :: [ExprAST] -> (Code, FEnv, VEnv, BEnv)
codeGens es = codeGens_ [] [] [] es (LCODE "")

-- Helper function for recursive code generation
codeGens_ ::
     FEnv -> VEnv -> BEnv -> [ExprAST] -> Code -> (Code, FEnv, VEnv, BEnv)
codeGens_ fenv venv benv [] code = (code, fenv, venv, benv)
codeGens_ fenv venv benv (e:es) code =
  codeGens_
    (_getFEnv e_result)
    (_getVEnv e_result)
    (_getBEnv e_result)
    es
    new_code
  where
    e_result = codeGen fenv venv benv e
    new_code = codeAppend code (_getCode e_result)

-- Function to generate code for a single expression
codeGen :: FEnv -> VEnv -> BEnv -> ExprAST -> (Code, FEnv, VEnv, BEnv)
codeGen fenv venv benv (NumberExprAST val) =
  (LCODE (show val), fenv, venv, benv)
codeGen fenv venv benv (VariableExprAST name) =
  case (lookup name venv) of
    Just (ACT val) -> (LCODE ("i32 " ++ (show val)), fenv, venv, benv)
    Just (SYM sym) -> (LCODE sym, fenv, venv, benv)
    Just (PTR ptl) -> (LCODE ptl, fenv, venv, benv)
    Nothing ->
      (ERROR ("Unknown variable name: name=" ++ name ++ "\n"), fenv, venv, benv)
codeGen fenv venv benv (BinaryExprAST op lhs rhs) =
  fst (codeGenBinaryExpr fenv venv benv (BinaryExprAST op lhs rhs))
codeGen fenv venv benv (CallExprAST fname argExprs) =
  case (lookup fname fenv) of
    Just argNames ->
      if (length argNames /= length argExprs)
        then (ERROR "Incorrect # arguments passed", fenv, venv, benv)
        else ( codeAppendN
                 intermediateCode
                 (createCall fname evaluatedArgs resultVar)
             , new_fenv
             , new_venv ++ [(resultVar, SYM resultVar)]
             , benv)
    Nothing -> (ERROR "Unknown functino referenced", fenv, venv, benv)
  where
    resultVar = generateNewVarName "%calltmp" venv
    (new_fenv, new_venv, intermediateCode, evaluatedArgs) =
      createArgs fenv venv benv argExprs
codeGen fenv venv benv (PrototypeAST fname argNames) =
  ( LCODE
      ("define i32 "
         ++ ("@" ++ fname')
         ++ ("(" ++ (joinWithCommaStr argNames) ++ ")"))
  , fenv ++ [(fname', argNames)]
  , addVarName argNames venv
  , benv)
  where
    fname' =
      if fname == ""
        then "0"
        else fname
codeGen fenv venv benv (FunctionAST prototype body) =
  case _getCode prototypeCODE of
    LCODE p ->
      case _getCode bodyCODE of
        LCODE b ->
          if (length localVars) == 0
            then ( ERROR "A function should return a value."
                 , newfenv
                 , localVars
                 , benv)
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
                 , localVars
                 , (_getBEnv bodyCODE))
        ERROR msg ->
          ( ERROR ("function body contains the following errors: " ++ msg)
          , fenv
          , venv
          , benv)
    ERROR msg ->
      ( ERROR ("function declaration contains the following errors: " ++ msg)
      , fenv
      , venv
      , benv)
  where
    prototypeCODE = codeGen fenv venv benv prototype
    newfenv = _getFEnv prototypeCODE
    bodyCODE =
      codeGen
        newfenv
        (venv ++ (_getVEnv prototypeCODE))
        (benv ++ [("entry", SYM "entry")])
        body
    localVars = _getVEnv bodyCODE
    retInstr_ = _getVal ((snd . last) localVars) venv
    retInstr =
      case (fst retInstr_) of
        "" -> ("", snd retInstr_)
        _ -> ("\n" ++ (fst retInstr_), snd retInstr_)
codeGen fenv venv benv (IfExprAST condAST thenAST elseAST) =
  ( code
  , fenv
  , (_getVEnv elseBranch) ++ [(iftmp, SYM iftmp)]
  , (_getBEnv elseBranch)
      ++ [ (label_then, SYM label_then)
         , (label_else, SYM label_else)
         , (label_ifcont, SYM label_ifcont)
         ])
  where
    label_then = generateNewBlockName "then" benv
    label_else = generateNewBlockName "else" benv
    label_ifcont = generateNewBlockName "ifcont" benv
    condCode = codeGen fenv venv benv condAST
    branchCode =
      LCODE
        ("\n\tbr i1 "
           ++ (fst . last . _getVEnv) condCode
           ++ ", label %"
           ++ label_then
           ++ ", label %"
           ++ label_else
           ++ "\n")
    entryCode = codeAppend (_getCode condCode) branchCode
    thenBranch =
      codeGen
        (_getFEnv condCode)
        (_getVEnv condCode)
        (_getBEnv condCode)
        thenAST
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
        [ LCODE ("\n" ++ label_then ++ ":")
        , thenBody
        , LCODE ("\n\tbr label %" ++ label_ifcont ++ "\n")
        ]
    elseBranch =
      codeGen
        (_getFEnv thenBranch)
        (_getVEnv thenBranch)
        (_getBEnv thenBranch)
        elseAST
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
        [ LCODE ("\n" ++ label_else ++ ":")
        , elseBody
        , LCODE ("\n\tbr label %" ++ label_ifcont ++ "\n")
        ]
    iftmp = generateNewVarName "%iftmp" (_getVEnv elseBranch)
    contCode =
      codeAppends
        [ LCODE ("\n" ++ label_ifcont ++ ":\n\t" ++ iftmp ++ " = phi i32 [ ")
        , thenRes
        , LCODE (", %" ++ label_then ++ " ], [ ")
        , elseRes
        , LCODE (", %" ++ label_else ++ " ]")
        ]
    code = codeAppends [entryCode, thenCode, elseCode, contCode]
codeGen fenv venv benv (ForAST startAST endAST stepAST bodyAST) =
  ( codeAppends [entrySection, loopSection, bodySection, exitSection]
  , fenv
  , [ (label_initialindex, SYM label_initialindex)
    , (label_nextindex, SYM label_nextindex)
    ]
      ++ (_getVEnv stepCode)
      ++ (if (length venv) == 0
            then []
            else [last venv])
  , (_getBEnv stepCode)
      ++ [ (label_loop, SYM label_loop)
         , (label_body, SYM label_body)
         , (label_exit_loop, SYM label_exit_loop)
         ])
  where
    label_loop = generateNewBlockName "loop" benv
    label_body = generateNewBlockName "body" benv
    label_exit_loop = generateNewBlockName "exit_loop" benv
    label_initialindex = generateNewVarName "initial_index" venv
    label_index = generateNewVarName "index" venv
    label_nextindex = generateNewVarName "next_index" venv
    startCode = codeGen fenv venv benv startAST
    counterVar = fst (last (_getVEnv startCode))
    entrySection =
      codeAppends
        [ (_getCode startCode)
        , LCODE
            ("\n\t%"
               ++ label_initialindex
               ++ " = load i32, i32* %"
               ++ counterVar
               ++ ", align 4")
        , LCODE ("\n\tbr label %" ++ label_loop ++ "\n")
        ]
    endCode =
      codeGen
        fenv
        ((_getVEnv startCode) ++ [(label_index, SYM ("%" ++ label_index))])
        (_getBEnv startCode)
        (replaceVarName endAST counterVar label_index)
    prev_entry = fst (last benv)
    loopSection =
      codeAppends
        [ LCODE (label_loop ++ ":\n")
        , LCODE
            ("\t%"
               ++ label_index
               ++ " = phi i32 [ "
               ++ "%"
               ++ label_initialindex
               ++ ", %"
               ++ prev_entry
               ++ " ], [ %"
               ++ label_nextindex
               ++ ", %"
               ++ label_body
               ++ " ]\n")
        , (_getCode endCode)
        , LCODE
            ("\n\tbr i1 "
               ++ ((fst . last . _getVEnv) endCode)
               ++ ", label %"
               ++ label_body
               ++ ", label %"
               ++ label_exit_loop
               ++ "\n")
        ]
    bodyCode =
      codeGen
        fenv
        (_getVEnv endCode)
        (_getBEnv endCode)
        (replaceVarName bodyAST counterVar label_index)
    stepCode =
      codeGen
        fenv
        (_getVEnv bodyCode)
        (_getBEnv bodyCode)
        (replaceVarName stepAST counterVar label_index)
    bodySection =
      codeAppends
        [ LCODE (label_body ++ ":\n")
        , (_getCode bodyCode)
        , LCODE "\n"
        , (_getCode stepCode)
        , LCODE
            ("\n\t%"
               ++ label_nextindex
               ++ " = add i32 "
               ++ ((fst . last . _getVEnv) stepCode)
               ++ ", 0")
        , LCODE ("\n\tbr label %" ++ label_loop ++ "\n")
        ]
    exitSection = LCODE (label_exit_loop ++ ":")
codeGen fenv venv benv (BlockAST []) = (LCODE "", fenv, venv, benv)
codeGen fenv venv benv (BlockAST (e:exprs)) =
  ( codeAppendN (_getCode e_result) (_getCode exprs_result)
  , _getFEnv exprs_result
  , _getVEnv exprs_result
  , _getBEnv exprs_result)
  where
    e_result = codeGen fenv venv benv e
    exprs_result =
      codeGen
        (_getFEnv e_result)
        (_getVEnv e_result)
        (_getBEnv e_result)
        (BlockAST exprs)
codeGen fenv venv benv ast = (errormsg, fenv, venv, benv)
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

replaceVarName :: ExprAST -> String -> String -> ExprAST
replaceVarName expr a b =
  case expr of
    NumberExprAST n -> NumberExprAST n
    VariableExprAST s ->
      if s == a
        then VariableExprAST b
        else VariableExprAST s
    BinaryExprAST op left right ->
      BinaryExprAST op (replaceVarName left a b) (replaceVarName right a b)
    CallExprAST name exprs ->
      CallExprAST name (map (\e -> replaceVarName e a b) exprs)
    FunctionAST proto body ->
      FunctionAST (replaceVarName proto a b) (replaceVarName body a b)
    IfExprAST cond els thn ->
      IfExprAST
        (replaceVarName cond a b)
        (replaceVarName els a b)
        (replaceVarName thn a b)
    ForAST start end step body ->
      ForAST
        (replaceVarName start a b)
        (replaceVarName end a b)
        (replaceVarName step a b)
        (replaceVarName body a b)
    BlockAST exprs -> BlockAST (map (\e -> replaceVarName e a b) exprs)
    x -> x

generateNewVarName :: String -> VEnv -> String
generateNewVarName w venv = generateNewVarName' w 0 venv

generateNewVarName' :: String -> Int -> VEnv -> String
generateNewVarName' w i venv =
  case (lookup (w ++ show i) venv) of
    Just _ -> generateNewVarName' w (i + 1) venv
    Nothing -> w ++ show i

generateNewBlockName :: String -> BEnv -> String
generateNewBlockName w benv = generateNewBlockName' w 0 benv

generateNewBlockName' :: String -> Int -> VEnv -> String
generateNewBlockName' w i venv =
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

createArgs :: FEnv -> VEnv -> BEnv -> [ExprAST] -> (FEnv, VEnv, Code, [Code])
createArgs ft nv bv [] = (ft, nv, LCODE "", [])
createArgs ft nv bv (a:aexprs) =
  ( n_ft
  , n_nv
  , codeAppendN a_intermediateCode n_intermediateCode
  , [a_evaluated] ++ n_evaluated)
  where
    a_intermediate = codeGen ft nv bv a
    (a_intermediateCode, a_evaluated) =
      case a of
        NumberExprAST num -> (LCODE "", LCODE (show num))
        VariableExprAST vname ->
          case lookup vname nv of
            Just (PTR ptr) -> (LCODE (fst load_ptr), LCODE (snd load_ptr))
              where load_ptr = _getVal (PTR ptr) nv
            _ -> (LCODE "", _getCode (codeGen ft nv bv a))
        _ ->
          ( _getCode (a_intermediate)
          , LCODE (fst (last (_getVEnv a_intermediate))))
    (n_ft, n_nv, n_intermediateCode, n_evaluated) =
      createArgs
        (_getFEnv a_intermediate)
        (_getVEnv a_intermediate)
        (_getBEnv a_intermediate)
        aexprs

codePrepStoreExpr ::
     FEnv -> VEnv -> BEnv -> ExprAST -> ((Code, FEnv, VEnv, BEnv), String)
codePrepStoreExpr fenv venv benv (BinaryExprAST '=' (VariableExprAST vname) rhs) =
  case lookup vname venv of
    Just _ -> ((LCODE "", fenv, venv, benv), "%" ++ vname)
    Nothing ->
      ( ( LCODE ("\t%" ++ vname ++ " = alloca i32, align 4\n")
        , fenv
        , venv ++ [(vname, PTR ("%" ++ vname))]
        , benv)
      , "%" ++ vname)
codePrepStoreExpr fenv venv benv _ =
  ( (ERROR "The left term of `=` should be a variable name.", fenv, venv, benv)
  , "")

codeGenBinaryExpr ::
     FEnv -> VEnv -> BEnv -> ExprAST -> ((Code, FEnv, VEnv, BEnv), String)
codeGenBinaryExpr fenv venv benv (BinaryExprAST op lhs rhs) =
  case op of
    '+' ->
      ( ( codeAppend intermediateCode (fADD lterm rterm newVar)
        , fenv
        , venvLR ++ [(newVar, SYM newVar)]
        , benv)
      , newVar)
    '-' ->
      ( ( codeAppend intermediateCode (fSUB lterm rterm newVar)
        , fenv
        , venvLR ++ [(newVar, SYM newVar)]
        , benv)
      , newVar)
    '*' ->
      ( ( codeAppend intermediateCode (fMUL lterm rterm newVar)
        , fenv
        , venvLR ++ [(newVar, SYM newVar)]
        , benv)
      , newVar)
    '<' ->
      ( ( codeAppend intermediateCode (fCmpULT lterm rterm newVar)
        , fenv
        , venvLR ++ [(newVar, SYM newVar)]
        , benv)
      , newVar)
    '=' ->
      ( ( codeAppends
            [ intermediateCodeR
            , (_getCode (fst prepStoreExpr))
            , LCODE "\tstore i32 "
            , rterm
            , LCODE (", i32* " ++ (snd prepStoreExpr) ++ ", align 4")
            ]
        , fenv
        , (_getVEnv (fst prepStoreExpr))
        , benv)
      , snd prepStoreExpr)
    _ -> ((ERROR "invalid binary operator", fenv, venvLR, benv), newVar)
  where
    lbranch = codeGenBinaryExpr fenv venv benv lhs
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
    rbranch = codeGenBinaryExpr fenv venvL benv rhs
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
    rvenv = _getVEnv (fst rbranch)
    rvenv_last =
      if (length rvenv) == 0
        then []
        else [last rvenv]
    prepStoreExpr =
      codePrepStoreExpr
        fenv
        (rvenv_last ++ venv)
        benv
        (BinaryExprAST op lhs rhs)
codeGenBinaryExpr fenv venv benv (CallExprAST fname argExprs) =
  (callResult, fst (last (_getVEnv callResult)))
  where
    callResult = codeGen fenv venv benv (CallExprAST fname argExprs)
codeGenBinaryExpr fenv venv benv (VariableExprAST x) =
  case lookup x venv of
    Just (PTR ptr) ->
      ( ( LCODE ("\t" ++ v ++ " = load i32, i32* " ++ ptr ++ ", align 4")
        , fenv
        , venv ++ [(v, SYM v)]
        , benv)
      , v)
      where v = generateNewVarName ptr venv
    _ -> (codeGen fenv venv benv (VariableExprAST x), "")
codeGenBinaryExpr fenv venv benv exp = (codeGen fenv venv benv exp, "")

joinWithComma :: [Code] -> String
joinWithComma [] = ""
joinWithComma [LCODE x] = "i32 " ++ x
joinWithComma ((LCODE x):xs) = "i32 " ++ x ++ ", " ++ joinWithComma xs

joinWithCommaStr :: [String] -> String
joinWithCommaStr [] = ""
joinWithCommaStr [x] = "i32 %" ++ x
joinWithCommaStr (x:xs) = "i32 %" ++ x ++ ", " ++ joinWithCommaStr xs
