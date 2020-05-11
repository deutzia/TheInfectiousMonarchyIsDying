# The Infectious Monarchy Is Dying

This is an interpreter of a fuctional programming language written in Haskell.
The main features are:
    * Lambda calculus based operations
    * Strong typing with type inference
    * Polymorphic algebraic datat types
    * Explicit laziness
    * Unique name

And yes, I had a lot of fun coming up with that name.

# Building and running

To build and run use cabal:
    * `cabal new-build` will build the binary
    * `cabal new-exec TheInfectiousMonarchyIsDying [filename]` will run the interpreter, either passing `filename` as a file to interpret or reading from stding if it's not provided (interpreting starts after receiving EOF though, so this is not very useful)

# Repo

All the code resides in `src/` directory, prelude is in `prelude.timid`, correct usage examples are located in `good/` and incorrect usage examples are located in `bad/` directory.

# TODO

I plan to rewrite `eval` to use `Reader` monad, add nicer error handling (with line numers and filenames), possibly with stacktrace. I would like to try to implement type classes, but it seems to be a fairly daunting task, so I find it rather unlikely to succeed.

# Not TODO

I've heard that syntax of the language is not graded, so I don't plan to fix using dot as a function application operator.
