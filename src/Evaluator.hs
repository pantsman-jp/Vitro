module Evaluator where

import Ast (Expr (..), Program (..), Statement (..))

lookupVar :: String -> [(String, Int)] -> Int
lookupVar x env = case lookup x env of
  Just v -> v
  Nothing -> error ("Undefined variable: " ++ x)

evalExpr :: [(String, Int)] -> Expr -> Int
evalExpr env (Lit n) = n
evalExpr env (Add a b) = evalExpr env a + evalExpr env b
evalExpr env (Sub a b) = evalExpr env a - evalExpr env b
evalExpr env (Mul a b) = evalExpr env a * evalExpr env b
evalExpr env (Div a b) = evalExpr env a `div` evalExpr env b
evalExpr env (Pow a b) = evalExpr env a ^ evalExpr env b
evalExpr env (Var x) = lookupVar x env

evalStmt :: [(String, Int)] -> Statement -> ([(String, Int)], Maybe Int)
evalStmt env (Assign x e) = ((x, evalExpr env e) : env, Nothing)
evalStmt env (Return e) = (env, Just (evalExpr env e))

evalProgram :: Program -> Int
evalProgram (Program stmts) = go [] stmts
  where
    go env [] = error "No return statement in program"
    go env (s : ss) = case evalStmt env s of
      (env', Just v) -> v
      (env', Nothing) -> go env' ss

eval :: Program -> Int
eval = evalProgram