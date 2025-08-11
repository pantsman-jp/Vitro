# Changelog of Vitro

## Will Be Implemented ...
- selection process (`if ~ then ~ else ~;`?)
- loop process (`while` or `for`)
- function
- string type
- float type

## v0.4.0 (2025-08-11)
- Renamed `Grammar.bnf` to `Grammar.ebnf`
  - Changed to use regular expressions
- Added grammar for sequence processing
  - Added corresponding AST nodes
  - Added parser implementation

## v0.3.0 (2025-08-06)
- split the project's module structure and organize it into three modules:
  1. Ast.hs
  1. Parser.hs
  1. Evaluator.hs
- define the Expr type representing the Abstract Syntax Tree (AST) in `Ast.hs`
- implement the parser in `Parser.hs` and modify it so that it can generate Expr from a string
- implemented the `eval :: Expr -> Int` function in `Evaluator.hs` to evaluate the AST
- added processing in `Main.hs` to parse and evaluate input strings
- added `docs/Grammar.bnf` to describe the BNF grammar

## v0.2.0 (2025-08-04)
- subtraction, division, and exponentiation

## v0.1.0 (2025-08-04)
- addition, multiplication, and expressions containing parentheses

## v0.0.0 (2025-08-04)
- start project