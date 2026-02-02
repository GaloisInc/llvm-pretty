# Revision history for llvm-pretty

## next

* Corrected the printing of non-normal single-precision floating point
  constants.

* Added missing `FloatType` case `BFloat` for 16-bit "Brain" floats.

* Added missing `Value'` cases for floating point types:
  * `ValHalf` containing an `FPHalfValue` (a wrapper around `Word16`)
  * `ValBFloat` containing an `FPBFloatValue` (a wrapper around `Word16`)
  * `ValFP128` containing an `FP128Value` (a wrapper around two `Word64`)
  * `ValFP128_PPC` containing an `FP128_PPCValue`
    (a wrapper around two `Double`s)

## 0.14.0.0 -- 2026-01-23

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
* Changes to cast-related instructions:
  * Add a `Bool` field to `ZExt` which, if `True`, indicates that the
    argument must be non-negative. This is used by LLVM 18 and up.
  * Add a `Bool` field to `UiToFp` which, if `True`, indicates that the
    argument must be non-negative. This is used by LLVM 19 and up.
  * Add `Bool` fields to `Trunc` to check if the truncation would cause
    unsigned or signed overflow. These are used by LLVM 20 and up.
* Add a `Bool` field to `ICmp`, which indicates that the arguments must have
  the same sign. This is used by LLVM 20 and up.
* Add `dlAtomGroup` and `dlAtomRank` fields to `DebugLoc'`, which were
  introduced in LLVM 21.
* Add `dilColumn`, `dilIsArtificial`, and `dilCoroSuspendIdx` fields to
  `DILabel'`, which were introduced in LLVM 21.
* The following debug-related fields have had their types changed from `Word64`
  to `Maybe (ValMd' lab)`:

  * `DIBasicType'`: `dibtSize`
  * `DICompositeType'`: `dictSize` and `dictOffset`
  * `DIDerivedType'`: `didtSize` and `didtOffset`

  This allows them to encode non-constant sizes and offsets (a capability used
  by Ada, for instance) in LLVM 21 or later.
* Added the `bbStmtModifier` which can be used to modify individual statements as
  they are emitted from the top-level instruction generator functions (e.g to add
  Debug Metadata to each statement).
* Fix a bug that would cause `indirectbr` statements to be pretty-printed
  incorrectly.

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
