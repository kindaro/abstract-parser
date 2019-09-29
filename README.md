# abstract-parser

## What should be achieved: _(Specific examples.)_

* Generating `PKGBUILD` files for a `git` repository at once.
* Parsing and printing `cabal` files exactly to easily improve `cabal init` code.
* Parsing and printing Literal Haskell and even any mix of languages and comments.

## Sections of function.

* Parse a writing with a given grammar.
* Print it back precisely.
* Serialize the syntax graph.
* Generate a random writing that conforms to the grammar.
* Interactively edit a writing that conforms to the grammar, or _(equivalently)_ its syntax tree.
* Address, search and replace with structural awareness.

## Plan of attack.

1. I need a tool that reads a grammar and says things about it. The more it has to say --- the
   better.
