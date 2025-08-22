module Evaluator where

import Ast (CompareOp (..), Expr (..), Process (..), Program (..), Statement (..))

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

evalCompare :: [(String, Int)] -> CompareOp -> Expr -> Expr -> Bool
evalCompare env Lt a b = evalExpr env a < evalExpr env b
evalCompare env Le a b = evalExpr env a <= evalExpr env b
evalCompare env Eq a b = evalExpr env a == evalExpr env b
evalCompare env Ne a b = evalExpr env a /= evalExpr env b
evalCompare env Ge a b = evalExpr env a >= evalExpr env b
evalCompare env Gt a b = evalExpr env a > evalExpr env b

evalProcess :: [(String, Int)] -> Process -> ([(String, Int)], Int)
evalProcess env (PVar x) = (env, lookupVar x env)
evalProcess env (PLit n) = (env, n)
evalProcess env (PExpr e) = (env, evalExpr env e)
evalProcess env (PIf assigns lhs op rhs thenP elseP) =
  let env' = foldl (\acc stmt -> fst (evalStmt acc stmt)) env assigns
      cond = evalCompare env' op lhs rhs
   in if cond
        then evalProcess env' thenP
        else evalProcess env' elseP

evalStmt :: [(String, Int)] -> Statement -> ([(String, Int)], Maybe Int)
evalStmt env (Assign x e) = ((x, evalExpr env e) : env, Nothing)
evalStmt env (Return p) =
  let (env', v) = evalProcess env p
   in (env', Just v)

evalProgram :: Program -> Int
evalProgram (Program stmts) = go [] stmts
  where
    go env [] = error "No return statement in program"
    go env (s : ss) = case evalStmt env s of
      (env', Just v) -> v
      (env', Nothing) -> go env' ss

eval :: Program -> Int
eval = evalProgram
