module Evaluator where

import Ast

eval :: Expr -> Int
eval (Lit n) = n
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b
eval (Div a b) = eval a `div` eval b
eval (Pow a b) = eval a ^ eval b
eval (Var _) = error "Variables not supported yet"
