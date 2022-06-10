# Lambda Calculator
> Untyped lambda calculus interpreter.

A simple implementation of the Untyped Lambda Calculus. It is written in Haskell and is
implemented to be as easy as possible to follow, at the possible expense of performance.

This project is intended to be an educational resource for learning and 
implementing functional programming languages.

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

    lambda-calculator
    
This will open a repl (read-eval-print loop) prompt

    Lambda Calculator (3.0.0.0)
    Type :h for help

You can start typing lambda calculus expressions and the program will evaluate them
and print the result. Here are a few examples:

    Lambda Calculator (0.5.0)
    Type :h for help
    λ > \x. x
    λx. x
    λ > (\x. x) n
    n
    λ > (\n f x. f (n f x)) (\f x. f (f x))
    λf x. f (f (f x))
    λ > :q
    
You can exit by typing the command :q.

## Running Tests
In order to run the testsuite, run

    stack test
    
# Author
**Sean Gillespie** [sean@mistersg.net](mailto:sean@mistersg.net)

# License
This project is licensed under the MIT License. See [LICENSE](LICENSE)
