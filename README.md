# The Variational Compiler

The Variational Compiler is an extension of source code which supports variational declarations, statements, and expressions, similar to C preprocessor directives. The planned usage of this compiler is to power an Atom editor package which will help users write and edit variational programs. We've pulled in Megaparsec to generate a parser and Aeson for communication between the Atom front-end and this compiler.

So far, we have a simple language definition and I/O setup, where programs can be streamed in on stdin and the JSON-encoded ASTs are streamed on stdout. Where we need to get is having projections (a set of pairs of dimension name and the choice for that dimension e.g. Left or Right) be streamed in on stdin and having something like the reduced AST streamed to stdout.

## How to run it

```bash
cabal install megaparsec aeson
ghc main.hs
./main < test1.in.vjava
./main < test2.in.vjava
```

or

```bash
ghci VariationalCompiler.hs
*VariationalCompiler> parseFromFile vjProgram "test1.in.vjava"
*VariationalCompiler> parseFromFile vjProgram "test2.in.vjava"
```

## Design questions

1. What kind of structure needs to be provided in order to support edit functionality?
  In this case, edit refers to taking the original program, a set of choices, and an edited projection of the original program using the provided set of choices, and producing the original program with the edits applied.
2. Does the AST need other things like line numbers and character positions to support syntax highlighting?
