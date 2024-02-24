import Lexer
import Parser

main :: IO ()
main = do
    let alpha = "abc123 .456 def abc.123 edf"
        resultAlpha1 = getAlpha alpha 0
        resultAlpha2 = getAlpha alpha 12
    putStrLn $ show (fst resultAlpha1) ++ ", " ++ show (snd resultAlpha1)
    putStrLn $ show (fst resultAlpha2) ++ ", " ++ show (snd resultAlpha2)
    
    let digit = "123.456 def abc0.123 edf"
        resultDigit1 = getDigit digit 0
        resultDigit2 = getDigit digit 15
    putStrLn $ show (fst resultDigit1) ++ ", " ++ show (snd resultDigit1)
    putStrLn $ show (fst resultDigit2) ++ ", " ++ show (snd resultDigit2)
