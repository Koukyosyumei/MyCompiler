import Generator
import Lexer
import Parser

check :: (Eq a, Show a) => String -> a -> a -> IO ()
check s c v = do
  if c == v
    then putStrLn $ s ++ ": OK"
    else putStrLn $ s ++ ": NG\na = " ++ show c ++ "\nb = " ++ show v

code2str :: Code -> String
code2str (LCODE code) = code
code2str (ERROR msg) = "ERROR: " ++ msg

main :: IO ()
main = do
    -- let alpha = "abc123 .456 def abc.123 edf"
    --    resultAlpha1 = getAlpha alpha 0
    --    resultAlpha2 = getAlpha alpha 12
    -- check "test1-1" (fst resultAlpha1) (TokIDENTIFIER "abc123")
    -- check "test1-2" (snd resultAlpha1) 6
    -- check "test1-3" (fst resultAlpha2) TokDEF
    -- check "test1-4" (snd resultAlpha2) 15
    -- let digit = "123.456 def abc0.123 edf"
    --    resultDigit1 = getDigit digit 0
    --    resultDigit2 = getDigit digit 15
    -- check "test2-1" (fst resultDigit1) (TokNUMBER 123.456)
    -- check "test2-2" (fst resultDigit2) (TokNUMBER 0.123)
  let source0 = "1+3; 2*3; } "
      sb0 = searchBlock source0 0 []
  check
    "test-block"
    sb0
    ( [ BinaryExprAST '+' (NumberExprAST 1) (NumberExprAST 3)
      , BinaryExprAST '*' (NumberExprAST 2) (NumberExprAST 3)
      ]
    , 12)
  let for0 = "def main() {for (a=1; a<2; a=a+1) in b=2;}"
      topfor0 = parseTop for0 0 []
  check
    "test-for"
    topfor0
    [ FunctionAST
        (PrototypeAST "main" [])
        (BlockAST
           [ ForAST
               (BinaryExprAST '=' (VariableExprAST "a") (NumberExprAST 1))
               (BinaryExprAST '<' (VariableExprAST "a") (NumberExprAST 2))
               (BinaryExprAST
                  '='
                  (VariableExprAST "a")
                  (BinaryExprAST '+' (VariableExprAST "a") (NumberExprAST 1)))
               (BinaryExprAST '=' (VariableExprAST "b") (NumberExprAST 2))
           ])
    ]
  let source1 = "def foo(x y) x+foo(y, 4);"
      top1 = parseTop source1 0 []
  check
    "test-SimplcCall"
    top1
    [ FunctionAST
        (PrototypeAST "foo" ["x", "y"])
        (BinaryExprAST
           '+'
           (VariableExprAST "x")
           (CallExprAST "foo" [VariableExprAST "y", NumberExprAST 4]))
    ]
  let source2 = "extern sin(a);"
      top2 = parseTop source2 0 []
  check "test-Extern" top2 [PrototypeAST "sin" ["a"]]
  let source3 = "4+5;"
      top3 = parseTop source3 0 []
      code3 = codeGen [] [] (top3 !! 0)
  check
    "test-SimpleAdd-AST"
    top3
    [ FunctionAST
        (PrototypeAST "" [])
        (BinaryExprAST '+' (NumberExprAST 4) (NumberExprAST 5))
    ]
  check
    "test-SimpleAdd-IR"
    (code2str (_getCode code3))
    "define i32 @0() {\nentry:\n\t%addtmp0 = add i32 4, 5\n\tret i32 %addtmp0\n}\n"
  let source4 = "def foo(a b) a*a+2*a*b+b*b;"
      top4 = parseTop source4 0 []
      code4 = codeGen [] [] (top4 !! 0)
      top4' = parseTop "def bar(a b) foo(a, 4)+bar(2, b);" 0 []
      code4' = codeGen (_getFEnv code4) (_getVEnv code4) (top4' !! 0)
  check
    "test-SimpleOp-AST"
    top4
    [ FunctionAST
        (PrototypeAST "foo" ["a", "b"])
        (BinaryExprAST
           '+'
           (BinaryExprAST
              '+'
              (BinaryExprAST '*' (VariableExprAST "a") (VariableExprAST "a"))
              (BinaryExprAST
                 '*'
                 (BinaryExprAST '*' (NumberExprAST 2) (VariableExprAST "a"))
                 (VariableExprAST "b")))
           (BinaryExprAST '*' (VariableExprAST "b") (VariableExprAST "b")))
    ]
  check
    "test-SimpleOp-IR"
    (code2str (_getCode code4))
    "define i32 @foo(i32 %a, i32 %b) {\nentry:\n\t%multmp0 = mul i32 %a, %a\n\t%multmp1 = mul i32 2, %a\n\t%multmp2 = mul i32 %multmp1, %b\n\t%addtmp0 = add i32 %multmp0, %multmp2\n\t%multmp3 = mul i32 %b, %b\n\t%addtmp1 = add i32 %addtmp0, %multmp3\n\tret i32 %addtmp1\n}\n"
  check
    "test-SimpleDefCall-IR"
    (code2str (_getCode code4'))
    "define i32 @bar(i32 %a, i32 %b) {\nentry:\n\t%calltmp0 = call i32 @foo(i32 %a, i32 4)\n\t%calltmp1 = call i32 @bar(i32 2, i32 %b)\n\t%addtmp2 = add i32 %calltmp0, %calltmp1\n\tret i32 %addtmp2\n}\n"
  let source5 = "def fib(x) if x<3 then 1 else fib(x-1)+fib(x-2);"
      top5 = parseTop source5 0 []
      code5 = codeGen [] [] (top5 !! 0)
  check
    "test-Fib-AST"
    top5
    [ FunctionAST
        (PrototypeAST "fib" ["x"])
        (IfExprAST
           (BinaryExprAST '<' (VariableExprAST "x") (NumberExprAST 3))
           (NumberExprAST 1)
           (BinaryExprAST
              '+'
              (CallExprAST
                 "fib"
                 [BinaryExprAST '-' (VariableExprAST "x") (NumberExprAST 1)])
              (CallExprAST
                 "fib"
                 [BinaryExprAST '-' (VariableExprAST "x") (NumberExprAST 2)])))
    ]
  check
    "test-Fib-IR"
    (code2str (_getCode code5))
    "define i32 @fib(i32 %x) {\nentry:\n\t%cmptmp0 = icmp ult i32 %x, 3\n\tbr i1 %cmptmp0, label %then, label %else\n\nthen:\n\tbr label %ifcont\n\nelse:\n\t%subtmp0 = sub i32 %x, 1\n\t%calltmp0 = call i32 @fib(i32 %subtmp0)\n\t%subtmp1 = sub i32 %x, 2\n\t%calltmp1 = call i32 @fib(i32 %subtmp1)\n\t%addtmp0 = add i32 %calltmp0, %calltmp1\n\tbr label %ifcont\n\nifcont:\n\t%iftmp = phi i32 [ 1, %then ], [ %addtmp0, %else ]\n\tret i32 %iftmp\n}\n"
