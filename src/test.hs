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
    check "test3-3-0" top3 [FunctionAST (PrototypeAST "" [])
                                    (BinaryExprAST '+' (NumberExprAST 4.0) (NumberExprAST 5.0))]
    check "test3-3-1" (code2str (_getCode code3)) "define double @0() {\nentry:\n\t%addtmp0 = fadd double 4.0, 5.0\n\tret double %addtmp0\n}\n"

    let source4 = "def foo(a b) a*a+2*a*b+b*b;"
        top4 = parseTop source4 0 []
        code4 = codeGen [] [] (top4 !! 0)
        top4' = parseTop "def bar(a b) foo(a, 4)+bar(2, b);" 0 []
        code4' = codeGen (_getFEnv code4) (_getVEnv code4) (top4' !! 0)
    check "test3-4-0" top4 [FunctionAST (PrototypeAST "foo" ["a","b"]) (BinaryExprAST '+' (BinaryExprAST '+' (BinaryExprAST '*' (VariableExprAST "a") (VariableExprAST "a")) (BinaryExprAST '*' (BinaryExprAST '*' (NumberExprAST 2.0) (VariableExprAST "a")) (VariableExprAST "b"))) (BinaryExprAST '*' (VariableExprAST "b") (VariableExprAST "b")))]
    check "test3-4-1" (code2str (_getCode code4)) "define double @foo(%a, %b) {\nentry:\n\t%multmp0 = fmul double %a, %a\n\t%multmp1 = fmul double 2.0, %a\n\t%multmp2 = fmul double %multmp1, %b\n\t%addtmp0 = fadd double %multmp0, %multmp2\n\t%multmp3 = fmul double %b, %b\n\t%addtmp1 = fadd double %addtmp0, %multmp3\n\tret double %addtmp1\n}\n"
    check "test3-4-2" (code2str (_getCode code4')) "define double @bar(%a, %b) {\nentry:\n\t%calltmp0 = call double @foo(double %a, double 4.0)\n\t%calltmp1 = call double @bar(double 2.0, double %b)\n\t%addtmp2 = fadd double %calltmp0, %calltmp1\n\tret double %addtmp2\n}\n"

    let source5 = "def fib(x) if x<3 then 1 else fib(x-1)+fib(x-2);"
        top5 = parseTop source5 0 []
        code5 = codeGen [] [] (top5 !! 0)
    check "test3-5-0" top5 [FunctionAST (PrototypeAST "fib" ["x"]) (IfExprAST (BinaryExprAST '<' (VariableExprAST "x") (NumberExprAST 3.0)) (NumberExprAST 1.0) (BinaryExprAST '+' (CallExprAST "fib" [BinaryExprAST '-' (VariableExprAST "x") (NumberExprAST 1.0)]) (CallExprAST "fib" [BinaryExprAST '-' (VariableExprAST "x") (NumberExprAST 2.0)])))]
    check "test3-5-1" (code2str (_getCode code5)) "define double @fib(%x) {\nentry:\n\t%cmptmp0 = fcmp ult double %x, 3.0\n\t%ifcond = %cmptmp0\n\tbr i1 %ifcond, label %then, label %else\n\nthen:\n\tbr label %ifcont\n\nelse:\n\t%calltmp0 = call double @fib(double %subtmp0 = fsub double %x, 1.0)\n\t%calltmp1 = call double @fib(double %subtmp0 = fsub double %x, 2.0)\n\t%addtmp0 = fadd double %calltmp0, %calltmp1\n\tbr label %ifcont\n\nifcont:\n\t%iftmp = phi double [ 1.0, %then ], [ %addtmp0, %else ]\n\tret double %iftmp\n}\n"

