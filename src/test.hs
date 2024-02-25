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

    let source = "def foo(x y) x+foo(y, 4.0);"
        top = parseTop source 0 []
        primary = parsePrimary source 12
        primary' = getTok source 12
        tmp1 = getTok "x+;" 1 
        -- binOpRHS = 
    putStrLn $ show top
    putStrLn $ show primary
    putStrLn $ show primary'
    putStrLn $ show tmp1
