import Evaluator
import Parser

main :: IO ()
main = do
  inp <- getLine
  case parse expr inp of
    [(ast, "")] -> print (eval ast)
    _ -> putStrLn "Parse error"
