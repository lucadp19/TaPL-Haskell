# TaPL-Haskell

`TaPL-Haskell` is an implementation of the various programming languages and type systems studied throughout the book *Types and Programming Languages* by Benjamin C. Pierce in Haskell.

Every language implementation will be completed by a parser and a REPL (Read-Eval-Print-Loop) to toy with the given language.

At the moment I've implemented:
- the simplest language, called `Arith` because it only supports a handful of arithmetic operations;
- the pure untyped lambda calculus, called `Untyped`;
- the full untyped lambda calculus, called `FullUntyped`;
- a typed implementation of the `Arith` language, called `TyArith`;
- a typed implementation of the Simply Typed Lambda Calculus, called `SimplyTyped`.

## Installation and usage

To try out the interpreters, first of all install `stack` from the package manager of your distribution (if you're on Windows you're on your own). For example if you're on Arch/Manjaro I suggest you should install `stack-static` from the AUR with any AUR helper (I'm using `yay`) to avoid the dynamic-linking-hell.

Then clone this repository into your computer and move into the directory. To use a particular language you first need to build the `stack` project, so issue the commands

    stack update
    stack build

At that point you can use the REPL of one of the languages by doing

    stack exec <executable-name>

(The name of the executables are in the table below.)

To open the documentation for the given language you can use the command

    stack haddock --open <executable-name>

More details inside each folder!

## Implemented languages

The following table summarises the implemented languages, the executable names, the TaPL chapter in which the implementation is described and which features the language does or does not implement.

| Language  name | Executable name | TaPL chapter | Nats  | Bools | Functions | Types |
|----------------|-----------------|--------------|-------|-------|-----------|-------|
| Arith          | arith           | 4            | ✔    | ✔    | ✗        | ✗    |
| Untyped        | untyped         | 7            | ✗    | ✗    | ✔        | ✗    |
| FullUntyped    | full-untyped    | 4 + 7        | ✔    | ✔    | ✔        | ✗    |
| TyArith        | typed-arith     | 8            | ✔    | ✔    | ✗        | ✔    |
| SimplyTyped    | simply-typed    | 10           | ✔    | ✔    | ✔        | ✔    |