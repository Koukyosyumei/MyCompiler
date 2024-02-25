import Lexer
import Parser

check :: (Eq a, Show a) => String -> a -> a -> IO()
check s c v = do
    if c == v
        then putStrLn $ s ++ ": OK" 
        else putStrLn $ s ++ ": NG (" ++ show c ++ " /= " ++ show v ++ ")"

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
    let source2 = "def foo(x y) x+y y;"
        top2 = parseTop source2 0 []
    putStrLn $ show top2

    let source3 = "def foo(x y) x+y );"
        top3 = parseTop source3 0 []
    putStrLn $ show top3

    let source4 = "extern sin(a);"
        top4 = parseTop source4 0 []
    putStrLn $ show top4
