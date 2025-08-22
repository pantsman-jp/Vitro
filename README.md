# Vitro / pantsman

## What is Vitro?
Vitro is a lightweight programming language that supports basic arithmetic, variables, and control structures.

### Arithmetic Operations
- Supports addition (`+`), subtraction (`-`), multiplication (`*`), division (`/`), and exponentiation (`^`).
- Parentheses `()` can be used to specify precedence.

### Variables
- Identifiers start with a lowercase letter and can include alphanumeric characters.
- Assignment is done using `x = 10;` syntax.

### Expressions and Processes
- Distinguishes between `Expr` and `Process`.
- `Process` can include literals, variables, parenthesized expressions, and `if` expressions.

### Conditional Expressions
- Supports `if <Expr> <CompareOp> <Expr> then <Process> else <Process>`.
- Comparison operators include `==`, `!=`, `<`, `<=`, `>`, `>=`.
- Both `then` and `else` clauses are mandatory.

### Return Values
- `return <Process>;` is used to produce the program's result.

### Syntax Flexibility
- Whitespace and line breaks are ignored for readability.
- Statements are separated by semicolons `;`.

### AST and Evaluation
- Generates an Abstract Syntax Tree (AST) for evaluation.
- `eval` function executes calculations while maintaining variable environment.

## Install
Download from <https://github.com/pantsman-jp/Vitro>.

## Usage
Require [GHCup](https://www.haskell.org/ghcup/). I use `ghcup 0.1.50.2`, `ghc 9.6.7`, `cabal 3.12.1.0`.

To run:
```shell
% cabal run
```
Then you can input a program, and it is evaluated.
Like this:
```
>
x=1; y=0; return if x==y then 1 else 0;
```
output is of course...
```
0
```

> [!NOTE]
> There is a line break after `> `.
> It's not a fatal bug.
> I will fix it when I feel like it.

## License
Copyright © 2025 pantsman