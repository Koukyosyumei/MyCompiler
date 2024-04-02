import Generator
import Lexer
import Parser
import System.Environment (getArgs)

code2str :: Code -> String
code2str (LCODE code) = code
code2str (ERROR msg) = "ERROR: " ++ msg

main :: IO ()
main = do
  args <- getArgs
  let source = head args
      top = parseTop source 0 []
      code = codeGens [] [] top
    -- putStrLn (show top)
  putStrLn (code2str (_getCode code))
