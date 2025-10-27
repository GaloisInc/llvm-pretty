# Revision history for llvm-pretty

## 0.14.0.0 (pending)

* Changes to support LLVM 19
  * Changes to `LayoutSpec` for DataLayout:
    * Add a `FunctionPointerAlign` constructor to `LayoutSpec`.
    * Size specification fields use a common sub-structure `Storage` which itself
      contains an `Alignment` common sub-structure: `IntegerSize`, `VectorSize`,
      `FloatSize`, and `StackObjSize`.
    * The pointer size specification field uses a `PointerSize` sub-structure
      that itself contains a `Storage` sub-structure.
    * Updated `AggregateSize` to make first field optional (it was dropped in
      LLVM 4) and the remaining fields are now provided via the `Alignment`
      sub-structure.
    * Added `ProgramAddrSpace`, `GlobalAddrSpace`, and `AllocaAddrSpace`
      constructors, each defined via an `AddressSpace` sub-structure.
    * Added `NonIntegralPointerSpaces` to record address spaces with an
      unspecified bitwise representation.
    * Added `GOFFMangling`, `WindowsX86CoffMangling`, and `XCOFFMangling` forms
      of Mangling.

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
