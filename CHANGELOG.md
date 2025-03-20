# Revision history for llvm-pretty

## 0.13.0.0 (March 2025)

* Changed some of the signatures of helper functions in the AST to make them more
  flexible by using `Type' ident` rather than `Type` in their signatures (the
  latter fixes `ident` to be `Ident`). Changed functions: `isAlias`,
  `isPrimTypeOf`, `isVector`, `isVectorOf`, `isArray`, and `isPointer`.

## 0.12.1.0 (August 2024)

* Fix for printing NaN and infinite floating point values.

* Add support for more AtomicRWOps.

## 0.12.0.0 (January 2024)

* Add preliminary support for LLVM versions up through 17.
