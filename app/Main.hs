import Parser

main :: IO ()
main = do
  putStrLn "> "
  line <- getLine
  case parse expr line of
    [(n, [])] -> print n
    [(_, out)] -> putStrLn ("illegal input" ++ out)
    [] -> putStrLn "illegal input"
    _ -> putStrLn "illegal input"