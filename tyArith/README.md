# TyArith

`TyArith` is a small statically typed programming language capable only of the most basic arithmetic and boolean operation. In particular it supports:
- an `if-then-else` construct
- `succ` and `prec` operators, to construct the natural numbers
- `isZero?`, which is an operator that tests if a number is zero or not.

The full BNF grammar for `TyArith` is the following:

    t ::= True
        | False
        | 0
        | succ t
        | prec t
        | isZero? t
        | if t then t else t
    
All `TyArith` programs are typed, which means that every ill-typed program is rejected before being executed: this is the main difference between `TyArith` and its predecessor, `Arith`.

Provided you've installed `stack`, `Arith` implements a simple REPL (Read-Eval-Print-Loop) with the following commands:
- `:p` parses an expression and prints out the corresponding AST;
- `:s` steps an expression into another expression. If the expression is either stuck or a value (which means it can't be evaluated further) the REPL prints a striked out arrow;
- `:a` fully evaluates an expression but prints all the steps;
- `:t` calculates the type of an expression;
- `:e` fully evaluates an expression and prints the final result (writing `:e` is the same as writing directly the expression);
- `:q` quits the REPL.

To open the `TyArith` REPL using `stack` you can use the command `stack exec tyArith-exe`. To open the documentation use the command `stack haddock --open tyArith` and `stack` will automatically open the Haddock files on your browser.
