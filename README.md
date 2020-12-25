# TaPL-Haskell

`TaPL-Haskell` is an implementation of the various programming languages and type systems studied throughout the book *Types and Programming Languages* by Benjamin C. Pierce in Haskell.

Every language implementation will be completed by a parser and a REPL (Read-Eval-Print-Loop) to toy with the given language.

At the moment I've only implemented the simplest language, called `Arith` because it only supports a handful of operators, but I plan on implementing all of them.

## Installation and usage

To try out the interpreters, first of all install `stack` from the package manager of your distribution (if you're on Windows you're on your own). For example if you're on Arch/Manjaro 

Then clone this repository into your computer and move into the directory. To use a particular language you first need to build the `stack` project, so move e.g. into the `Arith` folder and do 

    stack build

At that point you can use the REPL by doing

    stack exec arith-exe

(The name of the executable is `"language name"-exe` for every language)

More details inside each folder!