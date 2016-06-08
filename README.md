# The Variational Compiler

https://github.com/RikkiGibson/variational-compiler

The Variational Compiler is an extension of source code which supports variational declarations, statements, and expressions, similar to C preprocessor directives. The planned usage of this compiler is to power an Atom editor package which will help users write and edit variational programs. We've pulled in Megaparsec to generate a parser and Aeson for communication between the Atom front-end and this compiler.

Since the last milestone, we've added a "view" function to reduce ASTs, and we've refined the AST definitions as well as what attributes are on the AST nodes. We have a simple language definition set up and 2 different executables with work over stdin and stdout. The variational-parser executable takes a source program as input and produces the variational AST encoded in JSON as output. The variational-view executable takes a JSON value representing the AST and projection used to reduce the AST, and produces the reduced AST.

## How to run it

```bash
git clone https://github.com/RikkiGibson/variational-compiler.git
cd variational-compiler
# stack can be installed using `cabal install stack` or is available in many 
# system package managers (i.e., brew)
stack setup # don't be alarmed if this takes a few minutes
stack build
# Parse a file with the compiler
stack exec variational-parser < testfiles/hello.vjava | aeson-pretty
# Reduce an AST
stack exec variational-view < testfiles/view_input.json | aeson-pretty

```

## Design questions

1. What kind of structure needs to be provided in order to support edit functionality?
  In this case, edit refers to taking the original program, a set of choices, and an edited projection of the original program using the provided set of choices, and producing the original program with the edits applied.
2. What is an ideal JSON schema to represent the inputs and outputs to the program, and how should Haskell data types be structured to be amenable to this as well as maintainable?
