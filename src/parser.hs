module Parser where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Lexer

data ExprAST
  = NumberExprAST Int
  | VariableExprAST String
  | BinaryExprAST Char ExprAST ExprAST
  | CallExprAST String [ExprAST]
  | PrototypeAST String [String]
  | FunctionAST ExprAST ExprAST -- the first ExprAST should be PrototypeAST
  | IfExprAST ExprAST ExprAST ExprAST
  | BlockAST [ExprAST]
  | NullAST
  | Error String
  deriving (Eq, Show)

binoPrecedence :: [(Char, Int)]
binoPrecedence = [('=', 5), ('<', 10), ('+', 20), ('-', 20), ('*', 40)]

getTokPrecedence :: Char -> Int
getTokPrecedence c =
  case lookup c binoPrecedence of
    Nothing -> -1
    Just v -> v

isError :: ExprAST -> Bool
isError (Error _) = True
isError _ = False

-- top ::= definition | external | expression | {expression;*} | ;
parseTop :: String -> Int -> [ExprAST] -> [ExprAST]
parseTop s i es =
  case (fst tokAndpos) of
    TokEOF -> []
    (TokChar ';') -> es ++ (parseTop s (snd tokAndpos) es)
        -- (TokChar '{') -> es ++ [(fst parsedBLC)] ++ (parseTop s (snd parsedBLC) es)
    TokDEF -> es ++ [(fst parsedDEF)] ++ (parseTop s (snd parsedDEF) es)
    TokEXTERN -> es ++ [(fst parsedEXT)] ++ (parseTop s (snd parsedEXT) es)
    _ -> es ++ [(fst parsedTLE)] ++ (parseTop s (snd parsedTLE) es)
  where
    tokAndpos = getTok s i
        -- parsedBLC = parseBlock s i
    parsedDEF = parseDefinition s i
    parsedEXT = parseExtern s i
    parsedTLE = parseTopLevelExpr s i

parseNumberExpr :: Int -> Int -> (ExprAST, Int)
parseNumberExpr val i = (NumberExprAST val, i)

-- identifierexpr ::= identifier | identifier '(( expresion* ')'
parseIdentifierExpr :: String -> String -> Int -> (ExprAST, Int)
parseIdentifierExpr idName s i =
  if (fst nextTok == TokChar '(')
    then parseCallExpr idName [] s (snd nextTok)
    else (VariableExprAST idName, snd curTok)
  where
    curTok = getTok s i -- eat identifier
    nextTok = getTok s (snd curTok)

parseCallExpr :: String -> [ExprAST] -> String -> Int -> (ExprAST, Int)
parseCallExpr idName args s i =
  if (fst curTok == TokChar ')')
    then (CallExprAST idName args, snd curTok)
    else if (fst curTok == TokChar ',')
           then parseCallExpr idName args s (snd curTok)
           else case fst newArg of
                  Error e -> (Error e, snd newArg)
                  _ ->
                    parseCallExpr idName (args ++ [fst newArg]) s (snd newArg)
  where
    curTok = getTok s i
    newArg = parseExpression s i

-- parenexpr ::= '(' expression ')'
parseParentExpr :: String -> Int -> (ExprAST, Int)
parseParentExpr s i =
  if (fst v) == NullAST
    then v
    else if (s !! (i + 1)) /= ')'
           then (Error "expexted ')'", i)
           else (fst v, i + 1)
  where
    v = parseExpression s i

-- primary ::= identifierexpr | numberexpr | parenexpr
parsePrimary :: String -> Int -> (ExprAST, Int)
parsePrimary s i =
  case (fst curTok) of
    TokIDENTIFIER name -> parseIdentifierExpr name s i
    TokNUMBER val -> parseNumberExpr val (snd curTok)
    TokChar '(' -> parseParentExpr s (snd curTok)
    TokChar '{' -> parseBlock s (snd curTok)
    TokIF -> parseIfExpr s i
    _ ->
      ( Error
          ("unknown token when parsing a primary expression: "
             ++ show (fst curTok))
      , i)
  where
    curTok = getTok s i

-- binoprhs ::= (+ primary)*
parseBinOpRHS :: Int -> ExprAST -> String -> Int -> (ExprAST, Int)
parseBinOpRHS exprPrec lhs s i =
  if (length s) <= i
    then (lhs, i)
    else if tokPrec < exprPrec
           then (lhs, i)
           else if isError (fst rhs)
                  then rhs
                  else if tokPrec < nextPrec
                         then if isError (fst rhs')
                                then rhs'
                                else parseBinOpRHS
                                       exprPrec
                                       (BinaryExprAST binOp lhs (fst rhs'))
                                       s
                                       (snd rhs')
                         else parseBinOpRHS
                                exprPrec
                                (BinaryExprAST binOp lhs (fst rhs))
                                s
                                (snd rhs)
  where
    tokPrec = getTokPrecedence (s !! i)
    binOp = (s !! i)
    nextTok = getTok s i
    rhs = parsePrimary s (snd nextTok)
    nextPrec = getTokPrecedence (s !! (snd rhs))
    rhs' = parseBinOpRHS (tokPrec + 1) (fst rhs) s (snd rhs)

-- expression ::= primary binoprhs
parseExpression :: String -> Int -> (ExprAST, Int)
parseExpression s i =
  let lhs = parsePrimary s i
   in if (fst lhs) == NullAST
        then lhs
        else parseBinOpRHS 0 (fst lhs) s (snd lhs)

-- block ::= {expression;*}
parseBlock :: String -> Int -> (ExprAST, Int)
parseBlock s i = (BlockAST (fst blocks), snd blocks)
  where
    blocks = searchBlock s i []

searchBlock :: String -> Int -> [ExprAST] -> ([ExprAST], Int)
searchBlock s i exprs =
  if (length s <= i)
    then (exprs, i)
    else if fst curTok == TokChar '}'
           then (exprs, (snd curTok) + 1)
           else if fst curTok == TokChar ';'
                  then searchBlock
                         s
                         ((snd nextExp) + 1)
                         (exprs ++ [fst nextExp])
                  else searchBlock s ((snd curExp) + 1) (exprs ++ [fst curExp])
  where
    curTok = getTok s i
    curExp = parseExpression s i
    nextTok = getTok s ((snd curTok) + 1)
    nextExp = parseExpression s (snd nextTok)

parseBlockOrExpression :: String -> Int -> (ExprAST, Int)
parseBlockOrExpression s i =
  if fst curTok == TokChar '{'
    then parseBlock s ((snd curTok))
    else parseExpression s i
  where
    curTok = getTok s i

parsePrototype :: String -> Int -> (ExprAST, Int)
parsePrototype s i =
  case (fst curTok) of
    (TokIDENTIFIER fname) ->
      case (snd argNames) of
        (1) -> (Error "Expected '(' in prototype", snd argNames)
        (2) -> (Error "Expected ')' in prototype", snd argNames)
        _ -> (PrototypeAST fname (fst argNames), snd argNames)
    _ -> (Error "Expexted function name in prototype", snd argNames)
  where
    curTok = getTok s i -- fname
    argNames = parseArgNames s (snd curTok) -- `(` *argument `)`

parseArgNames :: String -> Int -> ([String], Int)
parseArgNames s i =
  if (s !! i) /= '('
    then ([], -1)
    else getArgNames s (i + 1) -- *arguments `)`
  where
    getArgNames :: String -> Int -> ([String], Int)
    getArgNames s i =
      case (fst curTok) of
        TokIDENTIFIER argname -> ([argname] ++ (fst nextArgs), snd nextArgs)
        TokChar ')' -> ([], (snd curTok))
        _ -> ([], -2)
      where
        curTok = getTok s i
        nextArgs = getArgNames s (snd curTok)

-- definition ::= 'def' prototype (expression | block)
parseDefinition :: String -> Int -> (ExprAST, Int)
parseDefinition s i =
  let proto = parsePrototype s (snd curTok)
   in case (fst proto) of
        Error msg -> (Error msg, snd proto)
        _ ->
          let e = parseBlockOrExpression s (snd proto)
           in case (fst e) of
                NullAST -> e
                Error _ -> e
                _ -> (FunctionAST (fst proto) (fst e), (snd e))
  where
    curTok = getTok s i -- eat `def`

-- external ::= `extern` prototype
parseExtern :: String -> Int -> (ExprAST, Int)
parseExtern s i = parsePrototype s (snd curTok)
  where
    curTok = getTok s i -- eat `extern`

-- ifexpr ::= 'if' expression 'then' (exprssion | block) 'else' (expression | block)
parseIfExpr :: String -> Int -> (ExprAST, Int)
parseIfExpr s i =
  case (fst condExpr) of
    Error msg ->
      (Error ("cond contains the following error: " ++ msg), snd condExpr)
    _ ->
      if fst curThen /= TokTHEN
        then (Error ("expected then, got " ++ show curThen), snd curThen)
        else if fst curElse /= TokELSE
               then (Error ("expected else " ++ show curElse), snd curElse)
               else ( IfExprAST (fst condExpr) (fst thenExpr) (fst elseExpr)
                    , snd elseExpr)
  where
    curIf = getTok s i -- eat `if`
    condExpr = parseExpression s (snd curIf)
    curThen = getTok s (snd condExpr)
    thenExpr = parseBlockOrExpression s (snd curThen)
    curElse = getTok s (snd thenExpr)
    elseExpr = parseBlockOrExpression s (snd curElse)

-- topLevelexpr
parseTopLevelExpr :: String -> Int -> (ExprAST, Int)
parseTopLevelExpr s i =
  case (fst e) of
    NullAST -> e
    Error msg -> e
    _ -> (FunctionAST (PrototypeAST "" []) (fst e), snd e)
  where
    e = parseBlockOrExpression s i
