import           Generator
import           Lexer
import           Parser

check :: (Eq a, Show a) => String -> a -> a -> IO()
check s c v = do
    if c == v
        then putStrLn $ s ++ ": OK"
        else putStrLn $ s ++ ": NG (" ++ show c ++ " /= " ++ show v ++ ")"

code2str :: Code -> String
code2str (LCODE code) = code
code2str (ERROR msg) = "ERROR: " ++ msg

main :: IO ()
main = do
    let alpha = "abc123 .456 def abc.123 edf"
        resultAlpha1 = getAlpha alpha 0
        resultAlpha2 = getAlpha alpha 12
    check "test1-1" (fst resultAlpha1) (TokIDENTIFIER "abc123")
    check "test1-2" (snd resultAlpha1) 6
    check "test1-3" (fst resultAlpha2) TokDEF
    check "test1-4" (snd resultAlpha2) 15

    let digit = "123.456 def abc0.123 edf"
        resultDigit1 = getDigit digit 0
        resultDigit2 = getDigit digit 15
    check "test2-1" (fst resultDigit1) (TokNUMBER 123.456)
    check "test2-2" (fst resultDigit2) (TokNUMBER 0.123)

    let source1 = "def foo(x y) x+foo(y, 4.0);"
        top1 = parseTop source1 0 []
    check "test3-1" top1 [FunctionAST (PrototypeAST "foo" ["x","y"])
                                     (BinaryExprAST '+' (VariableExprAST "x")
                                                        (CallExprAST "foo" [VariableExprAST "y",
                                                                            NumberExprAST 4.0]))]

    let source2 = "extern sin(a);"
        top2 = parseTop source2 0 []
    check "test3-2" top2 [PrototypeAST "sin" ["a"]]

    let source3 = "4+5;"
        top3 = parseTop source3 0 []
        code3 = codeGen [] [] (top3 !! 0)
    check "test3-3" top3 [FunctionAST (PrototypeAST "" [])
                                    (BinaryExprAST '+' (NumberExprAST 4.0) (NumberExprAST 5.0))]
    putStr (code2str code3)

    -- let esource3 = "def foo(x y) x+y );"
    --    etop3 = parseTop esource3 0 []
    -- putStrLn $ show etop3

