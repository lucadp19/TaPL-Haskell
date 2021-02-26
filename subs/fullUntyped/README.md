# Full Untyped Lambda Calculus

`FullUntyped` is an implementation of the Untyped Lambda Calculus with some additional operations on booleans and integers. In particular it supports:
- `lambda` abstractions and applications
- an `if-then-else` construct
- `succ` and `prec` operators, to construct the natural numbers
- `isZero?`, which is an operator that tests if a number is zero or not.

The full BNF grammar for `fullUntyped` is the following:
```
    <lit> ::= (<term>)
            | True
            | False
            | 0
            | <var>
    
    <app> ::= <lit>
            | <app> <lit>

    <complex> 
          ::= succ <app>
            | prec <app>
            | isZero? <app>
            | if <term>₁ then <term>₂ else <term>₃
            | \<var>. <term>
            | <app>
    
    <term> ::= <complex>
             | <term> <complex>
```
    
Provided you've installed `stack`, `fullUntyped` implements a REPL (Read-Eval-Print-Loop) with the following commands:
- `:help` (or the short version, `:h`) prints a message containing all the various REPL commands;
- `:parse` (`:p`) parses an expression and prints out the corresponding AST;
- `:step` (`:s`) steps an expression into another expression. If the expression is either stuck or a value (which means it can't be evaluated further) the REPL prints a striked out arrow;
- `:allSteps` (`:a`) fully evaluates an expression but prints all the steps;
- `:eval` (`:e`) fully evaluates an expression and prints the final result (writing `:e` is the same as writing directly the expression);
- `:let` (`:l`) binds a name to a term in the global environment: the bound name can be used in the following expressions. The exact syntax for this command is

        :l <name> = <expr>
- `:quit` (`:q`) quits the REPL.

To open the `FullUntyped` REPL using `stack` you should issue the following three commands:
```
$ stack update
$ stack build
$ stack exec full-untyped
```
The first updates the `stack` files, the second builds the `TaPL` project and the third finally launches the executable. 

To open the documentation use the command `stack haddock --open full-untyped` and `stack` will automatically open the Haddock files on your browser.
