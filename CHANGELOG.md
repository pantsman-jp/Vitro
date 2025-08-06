# Changelog of Vitro

## v0.3.0 (2025-08-06)
- Split the project's module structure and organize it into three modules:
  1. Ast.hs
  1. Parser.hs
  1. Evaluator.hs
- Define the Expr type representing the Abstract Syntax Tree (AST) in Ast.hs
- Implement the parser in Parser.hs and modify it so that it can generate Expr from a string
- Implemented the eval :: Expr -> Int function in Evaluator.hs to evaluate the AST
- Added processing in Main.hs to parse and evaluate input strings
- Added docs/Grammar.bnf to describe the BNF grammar

## v0.2.0 (2025-08-04)
- subtraction, division, and exponentiation

## v0.1.0 (2025-08-04)
- addition, multiplication, and expressions containing parentheses

## v0.0.0 (2025-08-04)
- start project