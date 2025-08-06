import Evaluator (eval)
import Parser (expr, parse)

main :: IO ()
main = do
  putStrLn "> "
  inp <- getLine
  case parse expr inp of
    [(ast, "")] -> print (eval ast)
    _ -> putStrLn "Parse error"