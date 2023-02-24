# Lambda Calculator
> Lambda Calculus and System F interpreter.

A simple implementation of the Untyped Lambda Calculus and System F. It is written in
Haskell and is implemented to be as easy as possible to follow, at the possible expense of
performance.

This project is intended to be an educational resource for learning and 
implementing functional programming languages.

## Installation

Lambda Calculator is on [Hackage](https://hackage.haskell.org/package/lambda-calculator-3.1.0.0):

    cabal install lambda-calculator

## Building
In order to build, you will need

 * GHC >= 8
 * stack

Build:

    stack build
    
Then install:

    stack install
    
## Running
Once the program is installed, you simply run it:

    lambda-calculator # Or,
    lambda-calculator --system-f
    
This will open a repl (read-eval-print loop) prompt

    Lambda Calculator (3.1.0.0)
    Type :h for help

You can start typing lambda calculus expressions and the program will evaluate them
and print the result. Here are a few examples:

    Lambda Calculator (3.1.0.0)
    Type :h for help
    λ > \x. x
    λx. x
    λ > (\x. x) n
    n
    λ > (\n f x. f (n f x)) (\f x. f (f x))
    λf x. f (f (f x))
    λ > :q
    
Here are some examples for the System F interpreter (`system-f`):

    Lambda Calculator (3.1.0.0)
    Type :h for help
    
    Λ > \x:T. x
    λ x:T. x : T -> T
    Λ > (\x:T. x) y:T
    y:T : T
    Λ > (\n:((T->T)->T->T) f:(T->T) x:T. f (n f x)) (\f:(T->T) x:T. x)
    λ f:(T->T) x:T. f x : (T -> T) -> T -> T
    Λ > :q
    
You can exit by typing the command :q.

## Running Tests
In order to run the testsuite, run

    stack test
    
# Author
**Sean Gillespie** [sean@mistersg.net](mailto:sean@mistersg.net)

# License
This project is licensed under the MIT License. See [LICENSE](LICENSE)
