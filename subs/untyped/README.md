# Untyped Lambda Calculus

`untyped` is an implementation of the pure Untyped Lambda Calculus: as such, it only implements lambda abstractions and applications.

The full BNF grammar for `untyped` is the following:

    t ::= \x. t
        | t1 t2
    
Provided you've installed `stack`, `untyped` implements a REPL (Read-Eval-Print-Loop) with the following commands:
- `:p` parses an expression and prints out the corresponding AST;
- `:s` steps an expression into another expression. If the expression is either stuck or a value (which means it can't be evaluated further) the REPL prints a striked out arrow;
- `:a` fully evaluates an expression but prints all the steps;
- `:e` fully evaluates an expression and prints the final result (writing `:e` is the same as writing directly the expression);
- `:l` binds a name to a term in the global environment: the bound name can be used in the following expressions. The exact syntax for this command is

        :l <name> = <expr>

- `:q` quits the REPL.

To open the `Untyped` REPL using `stack` you can use the command `stack exec untyped-exe`. To open the documentation use the command `stack haddock --open untyped` and `stack` will automatically open the Haddock files on your browser.

## Implementation details

Following the choices made in Types and Programming Languages, the `untyped` language uses a call-by-value interpreter: only the outmost lambda ever gets reduced, even if the body may be reduced before applying beta-substitution. 

Likewise, the interpreter actually performs the beta-substitutions instead of using a local environment: the `Eval` monad is used to keep track of local and global variables during parsing and pretty-printing, but not during the evaluation.