# The Infectious Monarchy Is Dying

This is an interpreter of a fuctional programming language written in Haskell.
The main features are:
* Lambda calculus based operations
* Strong typing with type inference
* Polymorphic algebraic data types
* Explicit laziness
* Unique name that refers to the condition of the world when this came to live

And yes, I had a lot of fun coming up with that name.

# Building and running

To build and run use cabal:
* `cabal new-build` will build the binary
* `cabal new-exec TheInfectiousMonarchyIsDying [filename]` will run the interpreter, either passing `filename` as a file to interpret or reading from stding if it's not provided (interpreting starts after receiving EOF though, so this is not very useful)

# Repo

All the code resides in `src/` directory, prelude is in `prelude.timid`, correct usage examples are located in `good/` and incorrect usage examples are located in `bad/` directory.

# TODO and not TODO

The grammar of the language is far from pretty - dot is used as a function application operator, which is probably the ugliest thing here. This is because syntax was not graded anyway, and I wanted to play around with megaparsec. It probably would be good to rewrite this, but the parser is not the most interesting part of the interpreter, so I focused on other features.
