# Roundabout


## Prerequisites

To work with HOPL3, you'll need the following:

 * [Haskell Platform](https://www.haskell.org/platform/)
 * [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)
 * Haskell `parsec` package (installs when you build with Haskell Stack)

You must also have the following additional tools:

 * [Git Source Code Management](http://git-scm.com/)
 * Command shell (such as BASH, Windows Command Prompt, etc.)
 * Your favorite programmer's plain-text editor (*not* Notepad!)

## Haskell Translation of _Essentials of Programming Languages_

This public repository provides two specific resources freely available
to anyone interested in Haskell as well as the EOPL3 approach to studying
the design and implementation of programming languages.

* [HOPL3 Codebase - Haskell Translation of EOPL3 Codebase](https://github.com/Marist-CMPT331L620F21/hopl3-code)
* [Companion Wiki for HOPL3](https://github.com/Marist-CMPT331L620F21/hopl3-code/wiki)

Additionally, to help my students manage using an alternate functional
programming language with the EOPL3 textbook, I have done my best to
translate the early Chapters of the book (from Ch. 1 to 3.2) into Haskell,
including all embedded code examples. This partial translation is contained
in a private repository only accessible to students in my class, who will
have their own copy of the original textbook from which to continue on to
the later Chapters. If you are enrolled in my class, then you can find
that private repository here:

* [Hasksentials of Programming Languages - Early Chapters](https://github.com/Marist-CMPT331L620F21/hopl3-book)

### Building and Running the Interpreter

In your Terminal or Git Bash, from the top-level directory of the repository,
run the following command, which will fetch all package dependencies and
compile the source code.

    $ stack build

There are then three different executables that you can generate: `hopl3-run`,
`hopl3-repl`, and `hopl3-test`.

To run the test suite:

    $ stack test

To run the interactive REPL:

    $ stack run hopl3-repl

To parse and evaluate a file containing a source program:

    $ stack run hopl3-run <FILE_CONTAINING_LANG_SOURCE>

We provide a sample input file named `test.in` for use with `hopl3-run`.
