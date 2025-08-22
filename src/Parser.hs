module Parser where

import Ast (CompareOp (..), Expr (..), Process (..), Program (..), Statement (..))
import Control.Applicative (Alternative (..))
import Data.Char (isAlpha, isAlphaNum, isDigit, isLower, isSpace, isUpper)

newtype Parser a = P (String -> [(a, String)])

instance Functor Parser where
  fmap g p =
    P
      ( \input -> case parse p input of
          [] -> []
          [(v, out)] -> [(g v, out)]
      )

instance Applicative Parser where
  pure v = P (\input -> [(v, input)])
  pg <*> px =
    P
      ( \input -> case parse pg input of
          [] -> []
          [(g, out)] -> parse (fmap g px) out
      )

instance Monad Parser where
  p >>= f =
    P
      ( \input -> case parse p input of
          [] -> []
          [(v, out)] -> parse (f v) out
      )

instance Alternative Parser where
  empty = P (const [])
  p <|> q =
    P
      ( \input -> case parse p input of
          [] -> parse q input
          [(v, out)] -> [(v, out)]
      )

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

item :: Parser Char
item =
  P
    ( \input -> case input of
        [] -> []
        (x : xs) -> [(x, xs)]
    )

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

digit, lower, upper, letter, alphanum :: Parser Char
digit = sat isDigit
lower = sat isLower
upper = sat isUpper
letter = sat isAlpha
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x : xs) = do
  char x
  string xs
  return (x : xs)

ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  return (x : xs)

nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

space :: Parser ()
space = do
  many (sat isSpace)
  return ()

int :: Parser Int
int =
  ( do
      char '-'
      n <- nat
      return (-n)
  )
    <|> nat

token :: Parser b -> Parser b
token p = do
  space
  v <- p
  space
  return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

expr :: Parser Expr
expr = do
  t <- term
  (do symbol "+"; Add t <$> expr)
    <|> (do symbol "-"; Sub t <$> expr)
    <|> return t

term :: Parser Expr
term = do
  p <- power
  (do symbol "*"; Mul p <$> term)
    <|> (do symbol "/"; Div p <$> term)
    <|> return p

power :: Parser Expr
power = do
  f <- factor
  (do symbol "^"; Pow f <$> power)
    <|> return f

factor :: Parser Expr
factor =
  (do symbol "("; e <- expr; symbol ")"; return e)
    <|> (Var <$> identifier)
    <|> (Lit <$> integer)

process :: Parser Process
process =
  ifStmt
    <|> (PLit <$> integer)
    <|> (PVar <$> identifier)
    <|> (do symbol "("; e <- expr; symbol ")"; return (PExpr e))
    <|> (PExpr <$> expr)

ifStmt :: Parser Process
ifStmt = do
  symbol "if"
  lhs <- expr
  op <- compareOp
  rhs <- expr
  symbol "then"
  p1 <- process
  symbol "else"
  p2 <- process
  return (PIf [] lhs op rhs p1 p2)

assignStmt :: Parser Statement
assignStmt = do
  var <- identifier
  symbol "="
  e <- expr
  symbol ";"
  return (Assign var e)

compareOp :: Parser CompareOp
compareOp =
  (symbol "<=" >> return Le)
    <|> (symbol "<" >> return Lt)
    <|> (symbol ">=" >> return Ge)
    <|> (symbol ">" >> return Gt)
    <|> (symbol "==" >> return Eq)
    <|> (symbol "!=" >> return Ne)

statement :: Parser Statement
statement =
  assignStmt
    <|> ( do
            symbol "return"
            p <- process
            symbol ";"
            return (Return p)
        )

program :: Parser Program
program = do
  space
  stmts <- many statement
  space
  return (Program stmts)

eof :: Parser ()
eof = P $ \input -> case input of
  [] -> [((), "")]
  _ -> []