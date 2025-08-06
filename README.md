# Vitro / pantsman

## What is Vitro?
Vitro is a name inspired by _in-vitro_ (it means `in a test tube`).  
Vitro is a simple interpreter implemented in Haskell that parses and evaluates mathematical expressions.  
The goal is to develop it into a general-purpose programming language in the future.

## Install
Download from <https://github.com/pantsman-jp/Vitro>.

## Usage
Require [GHCup](https://www.haskell.org/ghcup/).

I use `ghcup 0.1.50.2`, `ghc 9.6.7`, `cabal 3.12.1.0`.

To run
```shell
% cabal run
```
Then you can input an integer expression, and it is evaluated.
Like this:
```
>
1 + 2    *(5-2 /   1)
7
```
As you can see, the presence or absence of spaces between operators does not matter.  
You can add as many spaces as you like!!

There is a line break after `> `. I will fix it when I feel like it. It's not a fatal bug.

## License
Copyright © 2025 pantsman