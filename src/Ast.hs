module Ast where

newtype Program = Program [Statement] deriving (Show)

data Statement
  = Assign String Expr
  | Return Expr
  deriving (Show)

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  | Lit Int
  | Var String
  deriving (Show)
