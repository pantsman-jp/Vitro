# Vitro / pantsman

## What is Vitro?
Vitro is a simple interpreter implemented in Haskell that parses and evaluates mathematical expressions.  
Vitro is a name inspired by _in-vitro_ (it means `in a test tube`).  
The goal is to develop it into a general-purpose programming language in the future.

## Install
Download from [here](https://github.com/pantsman-jp/Vitro).

## Usage
Require [GHCup](https://www.haskell.org/ghcup/).

I use `ghcup 0.1.50.2`, `ghc 9.6.7`, `cabal 3.12.1.0`.

To run:
```shell
% cabal run
```
Then you can input an integer expression, and it is evaluated.
Like this:
```
>
1 + 2    *(3  ^  2 /3) - 4
3
```
As you can see, the presence or absence of spaces between operators does not matter.

> [!NOTE]
> There is a line break after `> `.
> It's not a fatal bug.
> I will fix it when I feel like it.

## License
Copyright © 2025 pantsman