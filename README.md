# The Variational Parser

https://github.com/RikkiGibson/variational-compiler

The Variational Parser is an extension of source code which supports variational declarations, statements, and expressions, similar to C preprocessor directives. The planned usage of this parser is to power an Atom editor package which will help users write and edit variational programs. We've pulled in **Megaparsec** to generate a parser and **Aeson** for communication between the Atom front-end and this compiler. We're using **Stack** as a build system, adhering roughly to the standard format for Haskell packages.

See the project report for more information about the design and capabilities of this package.

## How to run it
You will want to have stack installed to build this package.

```bash
git clone https://github.com/RikkiGibson/variational-compiler.git
cd variational-compiler
# stack can be installed using `cabal install stack` or is available in many
# system package managers (i.e., brew)
stack setup # don't be alarmed if this takes a few minutes
stack build

# Some example actions you can run:
# Parse a file with the compiler
stack exec variational-parser < testfiles/hello.vjava | aeson-pretty
# Reduce an AST
stack exec variational-view < testfiles/view_input.json | aeson-pretty
```
