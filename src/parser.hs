module Parser where
import           Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import           Lexer


data ExprAST = NumberExprAST Float
    | VariableExprAST String
    | BinaryExprAST Char ExprAST ExprAST
    | CallExprAST String [ExprAST]
    | PrototypeAST String [String]
    | FunctionAST ExprAST ExprAST  -- the first ExprAST should be PrototypeAST
    | NullAST
    | Error String
    deriving (Eq, Show)

binoPrecedence :: [(Char, Int)]
binoPrecedence = [('<', 10), ('+', 20), ('-', 20), ('*', 40)]

parseNumberExpr :: Float -> Int -> (ExprAST, Int)
parseNumberExpr val i = (NumberExprAST val, i)

-- parenexpr ::= '(' expression ')'
parseParentExpr :: String -> Int -> (ExprAST, Int)
parseParentExpr s i =
    if (fst v) == NullAST
        then v
    else if (s !! (i + 1)) /= ')'
        then (Error "expexted ')'", i)
    else
        (fst v, i + 1)
    where
        v = parseExpression s i

-- identifierexpr ::= identifier | identifier '(( expresion* ')'
parseIdentifierExpr :: String -> String -> Int -> (ExprAST, Int)
parseIdentifierExpr idName s i =
    if ((s !! (snd nextTok)) /= '(')
        then (VariableExprAST idName, snd nextTok)
        else parseCallExpr idName [] s (snd nextTok)
    where
        nextTok = getTok s i

parseCallExpr :: String -> [ExprAST] -> String -> Int -> (ExprAST, Int)
parseCallExpr idName args s i =
    if (s !! i) == ')'
        then (CallExprAST idName args, i + 1)
    else if (s !! i) == ','
        then let newArg = parseExpression s (i + 1) in
               case fst newArg of
                   NullAST -> (NullAST, snd newArg)
                   _ -> parseCallExpr idName (args ++ [fst newArg]) s (i + 1)
    else
        (Error "Call Error", i)

-- primary ::= identifierexpr | numberexpr | parenexpr
parsePrimary :: String -> Int -> (ExprAST, Int)
parsePrimary s i =
    case (fst curTok) of
        TokIDENTIFIER name -> parseIdentifierExpr name s (snd curTok)
        TokNUMBER val -> parseNumberExpr val (snd curTok)
        TokChar '(' -> parseParentExpr s (snd curTok)
        _ -> (Error "unknown token when expecting an expression", i)
    where
        curTok = getTok s i

getTokPrecedence :: Char -> Int
getTokPrecedence c =
    case lookup c binoPrecedence of
        Nothing -> -1
        Just v  -> v

isError :: ExprAST -> Bool
isError (Error _) = True
isError _ = False

-- binoprhs ::= (+ primary)*
parseBinOpRHS :: Int -> ExprAST -> String -> Int -> (ExprAST, Int)
parseBinOpRHS exprPrec lhs s i =
    if tokPrec < exprPrec
        then (lhs, i)
        else
        if isError (fst rhs)
            then rhs
            else if tokPrec < nextPrec
                then if isError (fst rhs')
                        then rhs'
                        else (BinaryExprAST binOp lhs (fst rhs'), snd rhs)
            else (BinaryExprAST binOp lhs (fst rhs), snd rhs)
    where
        tokPrec = getTokPrecedence (s !! i)
        binOp = (s !! (i + 1))
        rhs = parsePrimary s (i + 2)
        nextPrec = getTokPrecedence (s !! (snd rhs))
        rhs' = parseBinOpRHS (tokPrec + 1) (fst rhs) s (snd rhs)

-- expression ::= primary binoprhs
parseExpression :: String -> Int -> (ExprAST, Int)
parseExpression s i =
    let lhs = parsePrimary s i in
        if (fst lhs) == NullAST
            then lhs
            else parseBinOpRHS 0 (fst lhs) s i
