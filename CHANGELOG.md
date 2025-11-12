# Revision history for llvm-pretty

## 0.14.0.0 (pending)

* Changes to support LLVM 19 (some of these changes are not backward-compatible):
  * Changes to `LayoutSpec` for DataLayout:
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
    * Added `GoffMangling`, `WindowsX86CoffMangling`, and `XCoffMangling` forms
      of Mangling.
  * Added `GEPAttr` flags for `GEP` instruction and constant expression, with
    `RangeSpec` for the latter.
  * Added `numExtraInhabitants` to `DIBasicType`.
  * Added support for `DebugRecord` parsing, specifically for:

    * `FUNC_CODE_DEBUG_RECORD_VALUE`
    * `FUNC_CODE_DEBUG_RECORD_DECLARE`
    * `FUNC_CODE_DEBUG_RECORD_ASSIGN`
    * `FUNC_CODE_DEBUG_RECORD_VALUE_SIMPLE`
    * `FUNC_CODE_DEBUG_RECORD_LABEL`

    This adds a new field to both the `Result` and `Effect` constructors of the `Stmt` type.
  * Pretty-printing with an LLVM version >= 19 now generates an error for `icmp`,
    `fcmp`, and `shl` constant expressions that are no longer supported as of
    LLVM 19.

## 0.13.1.0 (October 2025)

* Add a `FunctionPointerAlign` constructor to `LayoutSpec`.

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
