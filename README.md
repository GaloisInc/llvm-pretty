# `llvm-pretty`

A pretty printing library that was inspired by the LLVM binding by Lennart
Augustsson. The library provides a monadic interface to a pretty printer, that
allows functions to be defined and called, generating the corresponding LLVM
assembly when run.

## LLVM language feature support

Currently, `llvm-pretty` supports LLVM versions up through 17. As a result of
the broad version coverage, the `llvm-pretty` AST is a superset of all versions
of the LLVM AST. This means that the manner in which certain information is
presented in the `llvm-pretty` AST (e.g., during pretty printing) will be
different depending on the LLVM version used to originate the information.
Conversely, it is possible to construct an `llvm-pretty` AST that cannot be
represented in a specific (or any) LLVM version.

`llvm-pretty` strives to support a reasonable variety of [LLVM language
features](https://llvm.org/docs/LangRef.html), but there are places where our
coverage of the LLVM language is incomplete. If you need a LLVM feature that is
not currently supported by `llvm-pretty`, please [file an
issue](https://github.com/elliottt/llvm-pretty/issues/new).

## `llvm-pretty` versus `llvm-pretty-bc-parser`

`llvm-pretty` supports almost everything that one would want to do with LLVM
ASTs. One notable exception is parsing: `llvm-pretty` deliberately does not
support parsing an LLVM module AST from a bitcode file. This functionality is
factored out into a separate
[`llvm-pretty-bc-parser`](https://github.com/GaloisInc/llvm-pretty-bc-parser)
library. `llvm-pretty-bc-parser` generally tries to stay in sync with all of
the LLVM language features that `llvm-pretty` supports, but it may be the case
that some valid `llvm-pretty` ASTs cannot be parsed by `llvm-pretty-bc-parser`.
If you encounter an occurrence of this issue, please [file an
issue](https://github.com/GaloisInc/llvm-pretty-bc-parser/issues/new) at the
`llvm-pretty-bc-parser` issue tracker.
