module Ast where

newtype Program = Program [Statement]

data Statement
  = Assign String Expr
  | Return Expr

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  | Lit Int
  | Var String
