# Arith

`Arith` is a small untyped programming language capable only of the most basic arithmetic and boolean operation. In particular it supports:
- an `if-then-else` construct
- `succ` and `prec` operators, to construct the natural numbers
- `isZero?`, which is an operator that tests if a number is zero or not
- and... basically that's it.

The full BNF grammar for Arith is the following:
```
    <lit> ::= (<term>)
            | True
            | False
            | 0

    <term> ::= succ <app>
             | prec <app>
             | isZero? <app>
             | if <term>₁ then <term>₂ else <term>₃
             | <lit>
```
    
Provided you've installed `stack`, `Arith` implements a simple but powerful REPL (Read-Eval-Print-Loop) with the following commands:
- `:p` parses an expression and prints out the corresponding AST;
- `:s` steps an expression into another expression. If the expression is either stuck or a value (which means it can't be evaluated further) the REPL prints a striked out arrow;
- `:a` fully evaluates an expression but prints all the steps;
- `:e` fully evaluates an expression and prints the final result (writing `:e` is the same as writing directly the expression);
- `:q` quits the REPL.

To open the `Arith` REPL using `stack` you should issue the following three commands:
```
$ stack update
$ stack build
$ stack exec arith
```
The first updates the `stack` files, the second builds the `TaPL` project and the third finally launches the executable. 

 To open the documentation use the command `stack haddock --open arith` and `stack` will automatically open the Haddock files on your browser.