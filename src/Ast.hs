module Ast where

newtype Program = Program [Statement]
  deriving (Show)

data Statement
  = Assign String Expr
  | Return Process
  deriving (Show)

data Process
  = PVar String
  | PLit Int
  | PExpr Expr
  | PIf [Statement] Expr CompareOp Expr Process Process
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

data CompareOp
  = Lt
  | Le
  | Eq
  | Ne
  | Ge
  | Gt
  deriving (Show)