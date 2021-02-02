# Arith

`Arith` is a small untyped programming language capable only of the most basic arithmetic and boolean operation. In particular it supports:
- an `if-then-else` construct
- `succ` and `prec` operators, to construct the natural numbers
- `isZero?`, which is an operator that tests if a number is zero or not
- and... basically that's it.

The full BNF grammar for Arith is the following:

    t ::= True
        | False
        | 0
        | succ t
        | prec t
        | isZero? t
        | if t then t else t
    
Provided you've installed `stack`, `Arith` implements a simple but powerful REPL (Read-Eval-Print-Loop) with the following commands:
- `:p` parses an expression and prints out the corresponding AST;
- `:s` steps an expression into another expression. If the expression is either stuck or a value (which means it can't be evaluated further) the REPL prints a striked out arrow;
- `:a` fully evaluates an expression but prints all the steps;
- `:e` fully evaluates an expression and prints the final result (writing `:e` is the same as writing directly the expression);
- `:q` quits the REPL.

To open the `Arith` REPL using `stack` you can use the command `stack exec arith-exe`. To open the documentation use the command `stack haddock --open arith` and `stack` will automatically open the Haddock files on your browser.
