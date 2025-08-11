import Evaluator (eval, evalExpr)
import Parser (expr, parse, program)

main :: IO ()
main = do
  putStrLn "> "
  inp <- getLine
  case parse program inp of
    [(ast, "")] -> print (eval ast)
    _ -> case parse expr inp of
      [(e, "")] -> print (evalExpr [] e)
      _ -> putStrLn "Parse error"
