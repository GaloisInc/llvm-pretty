{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

-- |
-- Module      :  Text.LLVM.PP
-- Copyright   :  Trevor Elliott 2011-2016
-- License     :  BSD3
--
-- Maintainer  :  awesomelyawesome@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- This is the pretty-printer for llvm assembly versions 3.6 and lower.
--
module Text.LLVM.PP where

import Text.LLVM.AST
import Text.LLVM.Triple.AST (TargetTriple)
import Text.LLVM.Triple.Print (printTriple)

import Control.Applicative ((<|>))
import Data.Bits ( shiftR, (.&.) )
import Data.Char (isAlphaNum,isAscii,isDigit,isPrint,ord,toUpper)
import Data.List (intersperse)
import qualified Data.Map as Map
import Data.Maybe (catMaybes,fromMaybe,isJust)
import Numeric (showHex)
import Text.PrettyPrint.HughesPJ
import Data.Int
import Prelude hiding ((<>))


-- Pretty-printer Config -------------------------------------------------------

type LLVM = ?config :: Config

-- | The differences between various versions of the llvm textual AST.
data Config = Config { cfgLoadImplicitType :: Bool
                       -- ^ True when the type of the result of a load is
                       -- derived from its pointer argument, or supplied
                       -- implicitly.

                     , cfgGEPImplicitType :: Bool
                       -- ^ True when the type of the result of the GEP
                       -- instruction is implied.

                     , cfgUseDILocation :: Bool
                     }

withConfig :: Config -> (LLVM => a) -> a
withConfig cfg body = let ?config = cfg in body


ppLLVM, ppLLVM35, ppLLVM36, ppLLVM37, ppLLVM38 :: (LLVM => a) -> a

ppLLVM = ppLLVM38

ppLLVM35 = ppLLVM36

ppLLVM36 = withConfig Config { cfgLoadImplicitType = True
                             , cfgGEPImplicitType  = True
                             , cfgUseDILocation    = False
                             }
ppLLVM37 = withConfig Config { cfgLoadImplicitType = False
                             , cfgGEPImplicitType  = False
                             , cfgUseDILocation    = True
                             }
ppLLVM38 = withConfig Config { cfgLoadImplicitType = False
                             , cfgGEPImplicitType  = False
                             , cfgUseDILocation    = True
                             }

checkConfig :: LLVM => (Config -> Bool) -> Bool
checkConfig p = p ?config


-- Modules ---------------------------------------------------------------------

ppModule :: LLVM => Module -> Doc
ppModule m = foldr ($+$) empty
  $ ppSourceName (modSourceName m)
  : ppTargetTriple (modTriple m)
  : ppDataLayout (modDataLayout m)
  : ppInlineAsm  (modInlineAsm m)
  : concat [ map ppTypeDecl    (modTypes m)
           , map ppGlobal      (modGlobals m)
           , map ppGlobalAlias (modAliases m)
           , map ppDeclare     (modDeclares m)
           , map ppDefine      (modDefines m)
           , map ppNamedMd     (modNamedMd m)
           , map ppUnnamedMd   (modUnnamedMd m)
           , map ppComdat      (Map.toList (modComdat m))
           ]


-- Source filename -------------------------------------------------------------

ppSourceName :: Maybe String -> Doc
ppSourceName Nothing   = empty
ppSourceName (Just sn) = "source_filename" <+> char '=' <+> doubleQuotes (text sn)

-- Metadata --------------------------------------------------------------------

ppNamedMd :: NamedMd -> Doc
ppNamedMd nm =
  sep [ ppMetadata (text (nmName nm)) <+> char '='
      , ppMetadata (braces (commas (map (ppMetadata . int) (nmValues nm)))) ]

ppUnnamedMd :: LLVM => UnnamedMd -> Doc
ppUnnamedMd um =
  sep [ ppMetadata (int (umIndex um)) <+> char '='
      , distinct <+> ppValMd (umValues um) ]
  where
  distinct | umDistinct um = "distinct"
           | otherwise     = empty


-- Aliases ---------------------------------------------------------------------

ppGlobalAlias :: LLVM => GlobalAlias -> Doc
ppGlobalAlias g = ppSymbol (aliasName g)
              <+> char '='
              <+> ppMaybe ppLinkage (aliasLinkage g)
              <+> ppMaybe ppVisibility (aliasVisibility g)
              <+> body
  where
  val  = aliasTarget g
  body = case val of
    ValSymbol _sym -> ppType (aliasType g) <+> ppValue val
    _              -> ppValue val


-- Target triple ---------------------------------------------------------------

-- | Pretty print a 'TargetTriple'
ppTargetTriple :: TargetTriple -> Doc
ppTargetTriple triple = "target" <+> "triple" <+> char '='
    <+> doubleQuotes (text (printTriple triple))

-- Data Layout -----------------------------------------------------------------

-- | Pretty print a data layout specification.
ppDataLayout :: DataLayout -> Doc
ppDataLayout [] = empty
ppDataLayout ls = "target" <+> "datalayout" <+> char '='
    <+> doubleQuotes (hcat (intersperse (char '-') (map ppLayoutSpec ls)))

-- | Pretty print a single layout specification.
ppLayoutSpec :: LayoutSpec -> Doc
ppLayoutSpec ls =
  case ls of
    BigEndian                 -> char 'E'
    LittleEndian              -> char 'e'
    PointerSize 0 sz abi pref -> char 'p' <> char ':' <> ppLayoutBody sz abi pref
    PointerSize n sz abi pref -> char 'p' <> int n <> char ':'
                                          <> ppLayoutBody sz abi pref
    IntegerSize   sz abi pref -> char 'i' <> ppLayoutBody sz abi pref
    VectorSize    sz abi pref -> char 'v' <> ppLayoutBody sz abi pref
    FloatSize     sz abi pref -> char 'f' <> ppLayoutBody sz abi pref
    StackObjSize  sz abi pref -> char 's' <> ppLayoutBody sz abi pref
    AggregateSize sz abi pref -> char 'a' <> ppLayoutBody sz abi pref
    NativeIntSize szs         ->
      char 'n' <> hcat (punctuate (char ':') (map int szs))
    StackAlign a              -> char 'S' <> int a
    Mangling m                -> char 'm' <> char ':' <> ppMangling m

-- | Pretty-print the common case for data layout specifications.
ppLayoutBody :: Int -> Int -> Maybe Int -> Doc
ppLayoutBody size abi mb = int size <> char ':' <> int abi <> pref
  where
  pref = case mb of
    Nothing -> empty
    Just p  -> char ':' <> int p

ppMangling :: Mangling -> Doc
ppMangling ElfMangling         = char 'e'
ppMangling MipsMangling        = char 'm'
ppMangling MachOMangling       = char 'o'
ppMangling WindowsCoffMangling = char 'w'


-- Inline Assembly -------------------------------------------------------------

-- | Pretty-print the inline assembly block.
ppInlineAsm :: InlineAsm -> Doc
ppInlineAsm  = foldr ($+$) empty . map ppLine
  where
  ppLine l = "module asm" <+> doubleQuotes (text l)


-- Identifiers -----------------------------------------------------------------

ppIdent :: Ident -> Doc
ppIdent (Ident n)
  | validIdentifier n = char '%' <> text n
  | otherwise         = char '%' <> ppStringLiteral n

-- | According to the LLVM Language Reference Manual, the regular
-- expression for LLVM identifiers is "[-a-zA-Z$._][-a-zA-Z$._0-9]".
-- Identifiers may also be strings of one or more decimal digits.
validIdentifier :: String -> Bool
validIdentifier [] = False
validIdentifier s@(c0 : cs)
  | isDigit c0 = all isDigit cs
  | otherwise  = all isIdentChar s
  where
  isIdentChar :: Char -> Bool
  isIdentChar c = isAlphaNum c || c `elem` ("-$._" :: [Char])


-- Symbols ---------------------------------------------------------------------

ppSymbol :: Symbol -> Doc
ppSymbol (Symbol n)
  | validIdentifier n = char '@' <> text n
  | otherwise         = char '@' <> ppStringLiteral n


-- Types -----------------------------------------------------------------------

ppPrimType :: PrimType -> Doc
ppPrimType Label          = "label"
ppPrimType Void           = "void"
ppPrimType (Integer i)    = char 'i' <> integer (toInteger i)
ppPrimType (FloatType ft) = ppFloatType ft
ppPrimType X86mmx         = "x86mmx"
ppPrimType Metadata       = "metadata"

ppFloatType :: FloatType -> Doc
ppFloatType Half      = "half"
ppFloatType Float     = "float"
ppFloatType Double    = "double"
ppFloatType Fp128     = "fp128"
ppFloatType X86_fp80  = "x86_fp80"
ppFloatType PPC_fp128 = "ppc_fp128"

ppType :: Type -> Doc
ppType (PrimType pt)     = ppPrimType pt
ppType (Alias i)         = ppIdent i
ppType (Array len ty)    = brackets (integral len <+> char 'x' <+> ppType ty)
ppType (PtrTo ty)        = ppType ty <> char '*'
ppType (Struct ts)       = structBraces (commas (map ppType ts))
ppType (PackedStruct ts) = angles (structBraces (commas (map ppType ts)))
ppType (FunTy r as va)   = ppType r <> ppArgList va (map ppType as)
ppType (Vector len pt)   = angles (integral len <+> char 'x' <+> ppType pt)
ppType Opaque            = "opaque"

ppTypeDecl :: TypeDecl -> Doc
ppTypeDecl td = ppIdent (typeName td) <+> char '='
            <+> "type" <+> ppType (typeValue td)


-- Declarations ----------------------------------------------------------------

ppGlobal :: LLVM => Global -> Doc
ppGlobal g = ppSymbol (globalSym g) <+> char '='
         <+> ppTheGlobalAttrs (globalAttrs g)
         <+> ppType (globalType g) <+> ppMaybe ppValue (globalValue g)
          <> ppAlign (globalAlign g)
          <> ppAttachedMetadata (Map.toList (globalMetadata g))
  where
  isStruct | Just (ValStruct {}) <- globalValue g = True
           | otherwise = False
  ppTheGlobalAttrs | isStruct = ppStructGlobalAttrs
                    | otherwise = ppGlobalAttrs

ppGlobalAttrs :: GlobalAttrs -> Doc
ppGlobalAttrs ga
    -- LLVM 3.8 does not emit or parse linkage information w/ hidden visibility
    | Just HiddenVisibility <- gaVisibility ga =
            ppVisibility HiddenVisibility <+> constant
    | otherwise = ppMaybe ppLinkage (gaLinkage ga) <+> ppMaybe ppVisibility (gaVisibility ga) <+> constant
  where
  constant | gaConstant ga = "constant"
           | otherwise     = "global"

ppStructGlobalAttrs :: GlobalAttrs -> Doc
ppStructGlobalAttrs ga
    -- LLVM 3.8 does not emit or parse external linkage for
    -- global structs
    | Just External <- gaLinkage ga,
      Just DefaultVisibility <- gaVisibility ga
        = constant
    | otherwise = ppGlobalAttrs ga
  where
  constant | gaConstant ga = "constant"
           | otherwise     = "global"

ppDeclare :: Declare -> Doc
ppDeclare d = "declare"
          <+> ppMaybe ppLinkage (decLinkage d)
          <+> ppMaybe ppVisibility (decVisibility d)
          <+> ppType (decRetType d)
          <+> ppSymbol (decName d)
           <> ppArgList (decVarArgs d) (map ppType (decArgs d))
          <+> hsep (ppFunAttr <$> decAttrs d)
          <> maybe empty ((char ' ' <>) . ppComdatName) (decComdat d)

ppComdatName :: String -> Doc
ppComdatName s = "comdat" <> parens (char '$' <> text s)

ppComdat :: (String,SelectionKind) -> Doc
ppComdat (n,k) = ppComdatName n <+> char '=' <+> text "comdat" <+> ppSelectionKind k

ppSelectionKind :: SelectionKind -> Doc
ppSelectionKind k =
    case k of
      ComdatAny             -> "any"
      ComdatExactMatch      -> "exactmatch"
      ComdatLargest         -> "largest"
      ComdatNoDuplicates    -> "noduplicates"
      ComdatSameSize        -> "samesize"

ppDefine :: LLVM => Define -> Doc
ppDefine d = "define"
         <+> ppMaybe ppLinkage (defLinkage d)
         <+> ppMaybe ppVisibility (defVisibility d)
         <+> ppType (defRetType d)
         <+> ppSymbol (defName d)
          <> ppArgList (defVarArgs d) (map (ppTyped ppIdent) (defArgs d))
         <+> hsep (ppFunAttr <$> defAttrs d)
         <+> ppMaybe (\s  -> "section" <+> doubleQuotes (text s)) (defSection d)
         <+> ppMaybe (\gc -> "gc" <+> ppGC gc) (defGC d)
         <+> ppMds (defMetadata d)
         <+> char '{'
         $+$ vcat (map ppBasicBlock (defBody d))
         $+$ char '}'
  where
  ppMds mdm =
    case Map.toList mdm of
      [] -> empty
      mds -> hsep [ "!" <> text k <+> ppValMd md | (k, md) <- mds ]

-- FunAttr ---------------------------------------------------------------------

ppFunAttr :: FunAttr -> Doc
ppFunAttr a =
  case a of
    AlignStack w    -> text "alignstack" <> parens (int w)
    Alwaysinline    -> text "alwaysinline"
    Builtin         -> text "builtin"
    Cold            -> text "cold"
    Inlinehint      -> text "inlinehint"
    Jumptable       -> text "jumptable"
    Minsize         -> text "minsize"
    Naked           -> text "naked"
    Nobuiltin       -> text "nobuiltin"
    Noduplicate     -> text "noduplicate"
    Noimplicitfloat -> text "noimplicitfloat"
    Noinline        -> text "noinline"
    Nonlazybind     -> text "nonlazybind"
    Noredzone       -> text "noredzone"
    Noreturn        -> text "noreturn"
    Nounwind        -> text "nounwind"
    Optnone         -> text "optnone"
    Optsize         -> text "optsize"
    Readnone        -> text "readnone"
    Readonly        -> text "readonly"
    ReturnsTwice    -> text "returns_twice"
    SanitizeAddress -> text "sanitize_address"
    SanitizeMemory  -> text "sanitize_memory"
    SanitizeThread  -> text "sanitize_thread"
    SSP             -> text "ssp"
    SSPreq          -> text "sspreq"
    SSPstrong       -> text "sspstrong"
    UWTable         -> text "uwtable"

-- Basic Blocks ----------------------------------------------------------------

ppLabelDef :: BlockLabel -> Doc
ppLabelDef (Named (Ident l)) = text l <> char ':'
ppLabelDef (Anon i)          = char ';' <+> "<label>:" <+> int i

ppLabel :: BlockLabel -> Doc
ppLabel (Named l) = ppIdent l
ppLabel (Anon i)  = char '%' <> int i

ppBasicBlock :: LLVM => BasicBlock -> Doc
ppBasicBlock bb = ppMaybe ppLabelDef (bbLabel bb)
              $+$ nest 2 (vcat (map ppStmt (bbStmts bb)))


-- Statements ------------------------------------------------------------------

ppStmt :: LLVM => Stmt -> Doc
ppStmt stmt = case stmt of
  Result var i mds -> ppIdent var <+> char '=' <+> ppInstr i
                   <> ppAttachedMetadata mds
  Effect i mds     -> ppInstr i <> ppAttachedMetadata mds

ppAttachedMetadata :: LLVM => [(String,ValMd)] -> Doc
ppAttachedMetadata mds
  | null mds  = empty
  | otherwise = comma <+> commas (map step mds)
  where
  step (l,md) = ppMetadata (text l) <+> ppValMd md


-- Linkage ---------------------------------------------------------------------

ppLinkage :: Linkage -> Doc
ppLinkage linkage = case linkage of
  Private                  -> "private"
  LinkerPrivate            -> "linker_private"
  LinkerPrivateWeak        -> "linker_private_weak"
  LinkerPrivateWeakDefAuto -> "linker_private_weak_def_auto"
  Internal                 -> "internal"
  AvailableExternally      -> "available_externally"
  Linkonce                 -> "linkonce"
  Weak                     -> "weak"
  Common                   -> "common"
  Appending                -> "appending"
  ExternWeak               -> "extern_weak"
  LinkonceODR              -> "linkonce_ddr"
  WeakODR                  -> "weak_odr"
  External                 -> "external"
  DLLImport                -> "dllimport"
  DLLExport                -> "dllexport"

ppVisibility :: Visibility -> Doc
ppVisibility v = case v of
    DefaultVisibility   -> "default"
    HiddenVisibility    -> "hidden"
    ProtectedVisibility -> "protected"

ppGC :: GC -> Doc
ppGC  = doubleQuotes . text . getGC


-- Expressions -----------------------------------------------------------------

ppTyped :: (a -> Doc) -> Typed a -> Doc
ppTyped fmt ty = ppType (typedType ty) <+> fmt (typedValue ty)

ppSignBits :: Bool -> Bool -> Doc
ppSignBits nuw nsw = opt nuw "nuw" <+> opt nsw "nsw"

ppExact :: Bool -> Doc
ppExact e = opt e "exact"

ppArithOp :: ArithOp -> Doc
ppArithOp (Add nuw nsw) = "add" <+> ppSignBits nuw nsw
ppArithOp FAdd          = "fadd"
ppArithOp (Sub nuw nsw) = "sub" <+> ppSignBits nuw nsw
ppArithOp FSub          = "fsub"
ppArithOp (Mul nuw nsw) = "mul" <+> ppSignBits nuw nsw
ppArithOp FMul          = "fmul"
ppArithOp (UDiv e)      = "udiv" <+> ppExact e
ppArithOp (SDiv e)      = "sdiv" <+> ppExact e
ppArithOp FDiv          = "fdiv"
ppArithOp URem          = "urem"
ppArithOp SRem          = "srem"
ppArithOp FRem          = "frem"

ppUnaryArithOp :: UnaryArithOp -> Doc
ppUnaryArithOp FNeg = "fneg"

ppBitOp :: BitOp -> Doc
ppBitOp (Shl nuw nsw) = "shl"  <+> ppSignBits nuw nsw
ppBitOp (Lshr e)      = "lshr" <+> ppExact e
ppBitOp (Ashr e)      = "ashr" <+> ppExact e
ppBitOp And           = "and"
ppBitOp Or            = "or"
ppBitOp Xor           = "xor"

ppConvOp :: ConvOp -> Doc
ppConvOp Trunc    = "trunc"
ppConvOp ZExt     = "zext"
ppConvOp SExt     = "sext"
ppConvOp FpTrunc  = "fptrunc"
ppConvOp FpExt    = "fpext"
ppConvOp FpToUi   = "fptoui"
ppConvOp FpToSi   = "fptosi"
ppConvOp UiToFp   = "uitofp"
ppConvOp SiToFp   = "sitofp"
ppConvOp PtrToInt = "ptrtoint"
ppConvOp IntToPtr = "inttoptr"
ppConvOp BitCast  = "bitcast"

ppAtomicOrdering :: AtomicOrdering -> Doc
ppAtomicOrdering Unordered = text "unordered"
ppAtomicOrdering Monotonic = text "monotonic"
ppAtomicOrdering Acquire   = text "acquire"
ppAtomicOrdering Release   = text "release"
ppAtomicOrdering AcqRel    = text "acq_rel"
ppAtomicOrdering SeqCst    = text "seq_cst"

ppAtomicOp :: AtomicRWOp -> Doc
ppAtomicOp AtomicXchg = "xchg"
ppAtomicOp AtomicAdd  = "add"
ppAtomicOp AtomicSub  = "sub"
ppAtomicOp AtomicAnd  = "and"
ppAtomicOp AtomicNand = "nand"
ppAtomicOp AtomicOr   = "or"
ppAtomicOp AtomicXor  = "xor"
ppAtomicOp AtomicMax  = "max"
ppAtomicOp AtomicMin  = "min"
ppAtomicOp AtomicUMax = "umax"
ppAtomicOp AtomicUMin = "umin"

ppScope ::  Maybe String -> Doc
ppScope Nothing = empty
ppScope (Just s) = "syncscope" <> parens (doubleQuotes (text s))

ppInstr :: LLVM => Instr -> Doc
ppInstr instr = case instr of
  Ret tv                 -> "ret" <+> ppTyped ppValue tv
  RetVoid                -> "ret void"
  Arith op l r           -> ppArithOp op <+> ppTyped ppValue l
                         <> comma <+> ppValue r
  UnaryArith op a        -> ppUnaryArithOp op <+> ppTyped ppValue a
  Bit op l r             -> ppBitOp op <+> ppTyped ppValue l
                         <> comma <+> ppValue r
  Conv op a ty           -> ppConvOp op <+> ppTyped ppValue a
                        <+> "to" <+> ppType ty
  Call tc ty f args      -> ppCall tc ty f args
  CallBr ty f args u es  -> ppCallBr ty f args u es
  Alloca ty len align    -> ppAlloca ty len align
  Load ptr mo ma         -> ppLoad ptr mo ma
  Store a ptr mo ma      -> ppStore a ptr mo ma
  Fence scope order      -> "fence" <+> ppScope scope <+> ppAtomicOrdering order
  CmpXchg w v p a n s o o' -> "cmpxchg" <+> opt w "weak"
                         <+> opt v "volatile"
                         <+> ppTyped ppValue p
                         <> comma <+> ppTyped ppValue a
                         <> comma <+> ppTyped ppValue n
                         <+> ppScope s
                         <+> ppAtomicOrdering o
                         <+> ppAtomicOrdering o'
  AtomicRW v op p a s o  -> "atomicrmw"
                         <+> opt v "volatile"
                         <+> ppAtomicOp op
                         <+> ppTyped ppValue p
                         <> comma <+> ppTyped ppValue a
                         <+> ppScope s
                         <+> ppAtomicOrdering o
  ICmp op l r            -> "icmp" <+> ppICmpOp op
                        <+> ppTyped ppValue l <> comma <+> ppValue r
  FCmp op l r            -> "fcmp" <+> ppFCmpOp op
                        <+> ppTyped ppValue l <> comma <+> ppValue r
  Phi ty vls             -> "phi" <+> ppType ty
                        <+> commas (map ppPhiArg vls)
  Select c t f           -> "select" <+> ppTyped ppValue c
                         <> comma <+> ppTyped ppValue t
                         <> comma <+> ppTyped ppValue (f <$ t)
  ExtractValue v is      -> "extractvalue" <+> ppTyped ppValue v
                         <> comma <+> (commas (map integral is))
  InsertValue a v is     -> "insertvalue" <+> ppTyped ppValue a
                         <> comma <+> ppTyped ppValue v
                         <> comma <+> commas (map integral is)
  ShuffleVector a b m    -> "shufflevector" <+> ppTyped ppValue a
                         <> comma <+> ppTyped ppValue (b <$ a)
                         <> comma <+> ppTyped ppValue m
  GEP ib ptr ixs         -> ppGEP ib ptr ixs
  Comment str            -> char ';' <+> text str
  Jump i                 -> "br"
                        <+> ppTypedLabel i
  Br c t f               -> "br" <+> ppTyped ppValue c
                         <> comma <+> ppType (PrimType Label)
                        <+> ppLabel t
                         <> comma <+> ppType (PrimType Label)
                        <+> ppLabel f
  Invoke ty f args to uw -> ppInvoke ty f args to uw
  Unreachable            -> "unreachable"
  Unwind                 -> "unwind"
  VaArg al t             -> "va_arg" <+> ppTyped ppValue al
                         <> comma <+> ppType t
  ExtractElt v i         -> "extractelement"
                        <+> ppTyped ppValue v
                         <> comma <+> ppVectorIndex i
  InsertElt v e i        -> "insertelement"
                        <+> ppTyped ppValue v
                         <> comma <+> ppTyped ppValue e
                         <> comma <+> ppVectorIndex i
  IndirectBr d ls        -> "indirectbr"
                        <+> ppTyped ppValue d
                         <> comma <+> commas (map ppTypedLabel ls)
  Switch c d ls          -> "switch"
                        <+> ppTyped ppValue c
                         <> comma <+> ppTypedLabel d
                        <+> char '['
                         $$ nest 2 (vcat (map (ppSwitchEntry (typedType c)) ls))
                         $$ char ']'
  LandingPad ty mfn c cs  ->
        case mfn of
            Just fn -> "landingpad"
                        <+> ppType ty
                        <+> "personality"
                        <+> ppTyped ppValue fn
                        $$ nest 2 (ppClauses c cs)
            Nothing -> "landingpad"
                        <+> ppType ty
                        $$ nest 2 (ppClauses c cs)
  Resume tv           -> "resume" <+> ppTyped ppValue tv
  Freeze tv           -> "freeze" <+> ppTyped ppValue tv

ppLoad :: LLVM => Typed (Value' BlockLabel) -> Maybe AtomicOrdering -> Maybe Align -> Doc
ppLoad ptr mo ma =
  "load" <+> (if isAtomic   then "atomic" else empty)
         <+> (if isImplicit then empty    else explicit)
         <+> ppTyped ppValue ptr
         <+> ordering
          <> ppAlign ma

  where
  isAtomic = isJust mo

  isImplicit = checkConfig cfgLoadImplicitType

  ordering =
    case mo of
      Just ao -> ppAtomicOrdering ao
      _       -> empty

  explicit =
    case typedType ptr of
      PtrTo ty -> ppType ty <> comma
      ty       -> ppType ty <> comma

ppStore :: LLVM
        => Typed (Value' BlockLabel)
        -> Typed (Value' BlockLabel)
        -> Maybe AtomicOrdering
        -> Maybe Align
        -> Doc
ppStore ptr val mo ma =
  "store" <+> (if isJust mo  then "atomic" else empty)
          <+> ppTyped ppValue ptr <> comma
          <+> ppTyped ppValue val
          <+> case mo of
                Just ao -> ppAtomicOrdering ao
                _       -> empty
          <> ppAlign ma


ppClauses :: LLVM => Bool -> [Clause] -> Doc
ppClauses isCleanup cs = vcat (cleanup : map ppClause cs)
  where
  cleanup | isCleanup = "cleanup"
          | otherwise = empty

ppClause :: LLVM => Clause -> Doc
ppClause c = case c of
  Catch  tv -> "catch"  <+> ppTyped ppValue tv
  Filter tv -> "filter" <+> ppTyped ppValue tv


ppTypedLabel :: BlockLabel -> Doc
ppTypedLabel i = ppType (PrimType Label) <+> ppLabel i

ppSwitchEntry :: Type -> (Integer,BlockLabel) -> Doc
ppSwitchEntry ty (i,l) = ppType ty <+> integer i <> comma <+> ppTypedLabel l

ppVectorIndex :: LLVM => Value -> Doc
ppVectorIndex i = ppType (PrimType (Integer 32)) <+> ppValue i

ppAlign :: Maybe Align -> Doc
ppAlign Nothing      = empty
ppAlign (Just align) = comma <+> "align" <+> int align

ppAlloca :: LLVM => Type -> Maybe (Typed Value) -> Maybe Int -> Doc
ppAlloca ty mbLen mbAlign = "alloca" <+> ppType ty <> len <> align
  where
  len = fromMaybe empty $ do
    l <- mbLen
    return (comma <+> ppTyped ppValue l)
  align = fromMaybe empty $ do
    a <- mbAlign
    return (comma <+> "align" <+> int a)

ppCall :: LLVM => Bool -> Type -> Value -> [Typed Value] -> Doc
ppCall tc ty f args
  | tc        = "tail" <+> body
  | otherwise = body
  where
  body = "call" <+> ppCallSym ty f
      <> parens (commas (map (ppTyped ppValue) args))

-- | Note that the textual syntax changed in LLVM 10 (@callbr@ was introduced in
-- LLVM 9).
ppCallBr :: LLVM => Type -> Value -> [Typed Value] -> BlockLabel -> [BlockLabel] -> Doc
ppCallBr ty f args to indirectDests =
  "callbr"
     <+> ppType res <+> ppValue f <> parens (commas (map (ppTyped ppValue) args))
     <+> "to" <+> ppLab to <+> brackets (commas (map ppLab indirectDests))
  where
    ppLab l = ppType (PrimType Label) <+> ppLabel l
    res =
      case ty of
        PtrTo (FunTy r _ _) -> r
        _ -> PrimType Void

ppCallSym :: LLVM => Type -> Value -> Doc
ppCallSym (PtrTo (FunTy res args va))   val        = ppType res <+> ppArgList va (map ppType args) <+> ppValue val
ppCallSym ty              val                      = ppType ty  <+> ppValue val

ppGEP :: LLVM => Bool -> Typed Value -> [Typed Value] -> Doc
ppGEP ib ptr ixs = "getelementptr" <+> inbounds
               <+> (if isImplicit then empty else explicit)
               <+> commas (map (ppTyped ppValue) (ptr:ixs))
  where
  isImplicit = checkConfig cfgGEPImplicitType

  explicit =
    case typedType ptr of
      PtrTo ty -> ppType ty <> comma
      ty       -> ppType ty <> comma

  inbounds | ib        = "inbounds"
           | otherwise = empty

ppInvoke :: LLVM => Type -> Value -> [Typed Value] -> BlockLabel -> BlockLabel -> Doc
ppInvoke ty f args to uw = body
  where
  body = "invoke" <+> ppType ty <+> ppValue f
      <> parens (commas (map (ppTyped ppValue) args))
     <+> "to" <+> ppType (PrimType Label) <+> ppLabel to
     <+> "unwind" <+> ppType (PrimType Label) <+> ppLabel uw

ppPhiArg :: LLVM => (Value,BlockLabel) -> Doc
ppPhiArg (v,l) = char '[' <+> ppValue v <> comma <+> ppLabel l <+> char ']'

ppICmpOp :: ICmpOp -> Doc
ppICmpOp Ieq  = "eq"
ppICmpOp Ine  = "ne"
ppICmpOp Iugt = "ugt"
ppICmpOp Iuge = "uge"
ppICmpOp Iult = "ult"
ppICmpOp Iule = "ule"
ppICmpOp Isgt = "sgt"
ppICmpOp Isge = "sge"
ppICmpOp Islt = "slt"
ppICmpOp Isle = "sle"

ppFCmpOp :: FCmpOp -> Doc
ppFCmpOp Ffalse = "false"
ppFCmpOp Foeq   = "oeq"
ppFCmpOp Fogt   = "ogt"
ppFCmpOp Foge   = "oge"
ppFCmpOp Folt   = "olt"
ppFCmpOp Fole   = "ole"
ppFCmpOp Fone   = "one"
ppFCmpOp Ford   = "ord"
ppFCmpOp Fueq   = "ueq"
ppFCmpOp Fugt   = "ugt"
ppFCmpOp Fuge   = "uge"
ppFCmpOp Fult   = "ult"
ppFCmpOp Fule   = "ule"
ppFCmpOp Fune   = "une"
ppFCmpOp Funo   = "uno"
ppFCmpOp Ftrue  = "true"

ppValue' :: LLVM => (i -> Doc) -> Value' i -> Doc
ppValue' pp val = case val of
  ValInteger i       -> integer i
  ValBool b          -> ppBool b
  ValFloat i         -> float i
  ValDouble i        -> double i
  ValFP80 (FP80_LongDouble e s) ->
    -- shown as 0xK<<20-hex-digits>>, per
    -- https://llvm.org/docs/LangRef.html#simple-constants
    let pad n | n < 0x10  = shows (0::Int) . showHex n
              | otherwise = showHex n
        fld v i = pad ((v `shiftR` (i * 8)) .&. 0xff)
    in "0xK" <> text (foldr (fld e) (foldr (fld s) "" $ reverse [0..7::Int]) [1, 0])
  ValIdent i         -> ppIdent i
  ValSymbol s        -> ppSymbol s
  ValNull            -> "null"
  ValArray ty es     -> brackets
                      $ commas (map (ppTyped (ppValue' pp) . Typed ty) es)
  ValVector ty es   -> angles $ commas
                     $ map (ppTyped (ppValue' pp) . Typed ty) es
  ValStruct fs       -> structBraces (commas (map (ppTyped (ppValue' pp)) fs))
  ValPackedStruct fs -> angles
                      $ structBraces (commas (map (ppTyped (ppValue' pp)) fs))
  ValString s        -> char 'c' <> ppStringLiteral (map (toEnum . fromIntegral) s)
  ValConstExpr ce    -> ppConstExpr' pp ce
  ValUndef           -> "undef"
  ValLabel l         -> pp l
  ValZeroInit        -> "zeroinitializer"
  ValAsm s a i c     -> ppAsm s a i c
  ValMd m            -> ppValMd' pp m
  ValPoison          -> "poison"

ppValue :: LLVM => Value -> Doc
ppValue = ppValue' ppLabel

ppValMd' :: LLVM => (i -> Doc) -> ValMd' i -> Doc
ppValMd' pp m = case m of
  ValMdString str   -> ppMetadata (ppStringLiteral str)
  ValMdValue tv     -> ppTyped (ppValue' pp) tv
  ValMdRef i        -> ppMetadata (int i)
  ValMdNode vs      -> ppMetadataNode' pp vs
  ValMdLoc l        -> ppDebugLoc' pp l
  ValMdDebugInfo di -> ppDebugInfo' pp di

ppValMd :: LLVM => ValMd -> Doc
ppValMd = ppValMd' ppLabel

ppDebugLoc' :: LLVM => (i -> Doc) -> DebugLoc' i -> Doc
ppDebugLoc' pp dl = (if cfgUseDILocation ?config then "!DILocation"
                                                 else "!MDLocation")
             <> parens (commas [ "line:"   <+> integral (dlLine dl)
                               , "column:" <+> integral (dlCol dl)
                               , "scope:"  <+> ppValMd' pp (dlScope dl)
                               ] <> mbIA <> mbImplicit)

  where
  mbIA = case dlIA dl of
           Just md -> comma <+> "inlinedAt:" <+> ppValMd' pp md
           Nothing -> empty
  mbImplicit = if dlImplicit dl then comma <+> "implicit" else empty

ppDebugLoc :: LLVM => DebugLoc -> Doc
ppDebugLoc = ppDebugLoc' ppLabel

ppTypedValMd :: LLVM => ValMd -> Doc
ppTypedValMd  = ppTyped ppValMd . Typed (PrimType Metadata)

ppMetadata :: Doc -> Doc
ppMetadata body = char '!' <> body

ppMetadataNode' :: LLVM => (i -> Doc) -> [Maybe (ValMd' i)] -> Doc
ppMetadataNode' pp vs = ppMetadata (braces (commas (map arg vs)))
  where arg = maybe ("null") (ppValMd' pp)

ppMetadataNode :: LLVM => [Maybe ValMd] -> Doc
ppMetadataNode = ppMetadataNode' ppLabel

ppStringLiteral :: String -> Doc
ppStringLiteral  = doubleQuotes . text . concatMap escape
  where
  escape c | c == '"' || c == '\\'  = '\\' : showHex (fromEnum c) ""
           | isAscii c && isPrint c = [c]
           | otherwise              = '\\' : pad (ord c)

  pad n | n < 0x10  = '0' : map toUpper (showHex n "")
        | otherwise =       map toUpper (showHex n "")

ppAsm :: Bool -> Bool -> String -> String -> Doc
ppAsm s a i c =
  "asm" <+> sideeffect <+> alignstack
        <+> ppStringLiteral i <> comma <+> ppStringLiteral c
  where
  sideeffect | s         = "sideeffect"
             | otherwise = empty

  alignstack | a         = "alignstack"
             | otherwise = empty


ppConstExpr' :: LLVM => (i -> Doc) -> ConstExpr' i -> Doc
ppConstExpr' pp expr =
  case expr of
    ConstGEP inb _mix mp ixs  ->
      "getelementptr"
        <+> opt inb "inbounds"
        <+> parens (mcommas ((ppType <$> mp) : (map (pure . ppTyp') ixs)))
    ConstConv op tv t  -> ppConvOp op <+> parens (ppTyp' tv <+> "to" <+> ppType t)
    ConstSelect c l r  ->
      "select" <+> parens (commas [ ppTyp' c, ppTyp' l , ppTyp' r])
    ConstBlockAddr t l -> "blockaddress" <+> parens (ppVal' (typedValue t) <> comma <+> pp l)
    ConstFCmp       op a b -> "fcmp" <+> ppFCmpOp op <+> ppTupleT a b
    ConstICmp       op a b -> "icmp" <+> ppICmpOp op <+> ppTupleT a b
    ConstArith      op a b -> ppArithOp op <+> ppTuple a b
    ConstUnaryArith op a   -> ppUnaryArithOp op <+> ppTyp' a
    ConstBit        op a b -> ppBitOp op   <+> ppTuple a b
  where ppTuple  a b = parens $ ppTyped ppVal' a <> comma <+> ppVal' b
        ppTupleT a b = parens $ ppTyped ppVal' a <> comma <+> ppTyp' b
        ppVal'       = ppValue' pp
        ppTyp'       = ppTyped ppVal'

ppConstExpr :: LLVM => ConstExpr -> Doc
ppConstExpr = ppConstExpr' ppLabel

-- DWARF Debug Info ------------------------------------------------------------

ppDebugInfo' :: LLVM => (i -> Doc) -> DebugInfo' i -> Doc
ppDebugInfo' pp di = case di of
  DebugInfoBasicType bt         -> ppDIBasicType bt
  DebugInfoCompileUnit cu       -> ppDICompileUnit' pp cu
  DebugInfoCompositeType ct     -> ppDICompositeType' pp ct
  DebugInfoDerivedType dt       -> ppDIDerivedType' pp dt
  DebugInfoEnumerator nm v u    -> ppDIEnumerator nm v u
  DebugInfoExpression e         -> ppDIExpression e
  DebugInfoFile f               -> ppDIFile f
  DebugInfoGlobalVariable gv    -> ppDIGlobalVariable' pp gv
  DebugInfoGlobalVariableExpression gv -> ppDIGlobalVariableExpression' pp gv
  DebugInfoLexicalBlock lb      -> ppDILexicalBlock' pp lb
  DebugInfoLexicalBlockFile lbf -> ppDILexicalBlockFile' pp lbf
  DebugInfoLocalVariable lv     -> ppDILocalVariable' pp lv
  DebugInfoSubprogram sp        -> ppDISubprogram' pp sp
  DebugInfoSubrange sr          -> ppDISubrange sr
  DebugInfoSubroutineType st    -> ppDISubroutineType' pp st
  DebugInfoNameSpace ns         -> ppDINameSpace' pp ns
  DebugInfoTemplateTypeParameter dttp  -> ppDITemplateTypeParameter' pp dttp
  DebugInfoTemplateValueParameter dtvp -> ppDITemplateValueParameter' pp dtvp
  DebugInfoImportedEntity diip         -> ppDIImportedEntity' pp diip
  DebugInfoLabel dil            -> ppDILabel' pp dil
  DebugInfoArgList args         -> ppDIArgList' pp args

ppDebugInfo :: LLVM => DebugInfo -> Doc
ppDebugInfo = ppDebugInfo' ppLabel

ppDIImportedEntity' :: LLVM => (i -> Doc) -> DIImportedEntity' i -> Doc
ppDIImportedEntity' pp ie = "!DIImportedEntity"
  <> parens (mcommas [ pure ("tag:"    <+> integral (diieTag ie))
                     , (("scope:"  <+>) . ppValMd' pp) <$> diieScope ie
                     , (("entity:" <+>) . ppValMd' pp) <$> diieEntity ie
                     , (("file:"   <+>) . ppValMd' pp) <$> diieFile ie
                     , pure ("line:"   <+> integral (diieLine ie))
                     , (("name:"   <+>) . text)        <$> diieName ie
                     ])

ppDIImportedEntity :: LLVM => DIImportedEntity -> Doc
ppDIImportedEntity = ppDIImportedEntity' ppLabel

ppDILabel' :: LLVM => (i -> Doc) -> DILabel' i -> Doc
ppDILabel' pp ie = "!DILabel"
  <> parens (mcommas [ (("scope:"  <+>) . ppValMd' pp) <$> dilScope ie
                     , pure ("name:" <+> text (dilName ie))
                     , (("file:"   <+>) . ppValMd' pp) <$> dilFile ie
                     , pure ("line:"   <+> integral (dilLine ie))
                     ])

ppDILabel :: LLVM => DILabel -> Doc
ppDILabel = ppDILabel' ppLabel

ppDINameSpace' :: LLVM => (i -> Doc) -> DINameSpace' i -> Doc
ppDINameSpace' pp ns = "!DINameSpace"
  <> parens (mcommas [ ("name:"   <+>) . text <$> (dinsName ns)
                     , pure ("scope:"  <+> ppValMd' pp (dinsScope ns))
                     , pure ("file:"   <+> ppValMd' pp (dinsFile ns))
                     , pure ("line:"   <+> integral (dinsLine ns))
                     ])

ppDINameSpace :: LLVM => DINameSpace -> Doc
ppDINameSpace = ppDINameSpace' ppLabel

ppDITemplateTypeParameter' :: LLVM => (i -> Doc) -> DITemplateTypeParameter' i -> Doc
ppDITemplateTypeParameter' pp tp = "!DITemplateTypeParameter"
  <> parens (mcommas [ ("name:"  <+>) . text        <$> dittpName tp
                     , ("type:"  <+>) . ppValMd' pp <$> dittpType tp
                     ])

ppDITemplateTypeParameter :: LLVM => DITemplateTypeParameter -> Doc
ppDITemplateTypeParameter = ppDITemplateTypeParameter' ppLabel

ppDITemplateValueParameter' :: LLVM => (i -> Doc) -> DITemplateValueParameter' i -> Doc
ppDITemplateValueParameter' pp vp = "!DITemplateValueParameter"
  <> parens (mcommas [ pure ("tag:"   <+> integral (ditvpTag vp))
                     , ("name:"  <+>) . text        <$> ditvpName vp
                     , ("type:"  <+>) . ppValMd' pp <$> ditvpType vp
                     , pure ("value:" <+> ppValMd' pp (ditvpValue vp))
                     ])

ppDITemplateValueParameter :: LLVM => DITemplateValueParameter -> Doc
ppDITemplateValueParameter = ppDITemplateValueParameter' ppLabel

ppDIBasicType :: DIBasicType -> Doc
ppDIBasicType bt = "!DIBasicType"
  <> parens (commas [ "tag:"      <+> integral (dibtTag bt)
                    , "name:"     <+> doubleQuotes (text (dibtName bt))
                    , "size:"     <+> integral (dibtSize bt)
                    , "align:"    <+> integral (dibtAlign bt)
                    , "encoding:" <+> integral (dibtEncoding bt)
                    ] <> mbFlags)
  where
  mbFlags = case dibtFlags bt of
              Just flags -> comma <+> "flags:" <+> integral flags
              Nothing -> empty

ppDICompileUnit' :: LLVM => (i -> Doc) -> DICompileUnit' i -> Doc
ppDICompileUnit' pp cu = "!DICompileUnit"
  <> parens (mcommas
       [ pure ("language:"           <+> integral (dicuLanguage cu))
       ,     (("file:"               <+>) . ppValMd' pp) <$> (dicuFile cu)
       ,     (("producer:"           <+>) . doubleQuotes . text)
             <$> (dicuProducer cu)
       , pure ("isOptimized:"        <+> ppBool (dicuIsOptimized cu))
       , pure ("flags:"              <+> ppFlags (dicuFlags cu))
       , pure ("runtimeVersion:"     <+> integral (dicuRuntimeVersion cu))
       ,     (("splitDebugFilename:" <+>) . doubleQuotes . text)
             <$> (dicuSplitDebugFilename cu)
       , pure ("emissionKind:"       <+> integral (dicuEmissionKind cu))
       ,     (("enums:"              <+>) . ppValMd' pp) <$> (dicuEnums cu)
       ,     (("retainedTypes:"      <+>) . ppValMd' pp) <$> (dicuRetainedTypes cu)
       ,     (("subprograms:"        <+>) . ppValMd' pp) <$> (dicuSubprograms cu)
       ,     (("globals:"            <+>) . ppValMd' pp) <$> (dicuGlobals cu)
       ,     (("imports:"            <+>) . ppValMd' pp) <$> (dicuImports cu)
       ,     (("macros:"             <+>) . ppValMd' pp) <$> (dicuMacros cu)
       , pure ("dwoId:"              <+> integral (dicuDWOId cu))
       ])

ppDICompileUnit :: LLVM => DICompileUnit -> Doc
ppDICompileUnit = ppDICompileUnit' ppLabel

ppFlags :: Maybe String -> Doc
ppFlags mb = doubleQuotes (maybe empty text mb)

ppDICompositeType' :: LLVM => (i -> Doc) -> DICompositeType' i -> Doc
ppDICompositeType' pp ct = "!DICompositeType"
  <> parens (mcommas
       [ pure ("tag:"            <+> integral (dictTag ct))
       ,     (("name:"           <+>) . doubleQuotes . text) <$> (dictName ct)
       ,     (("file:"           <+>) . ppValMd' pp) <$> (dictFile ct)
       , pure ("line:"           <+> integral (dictLine ct))
       ,     (("baseType:"       <+>) . ppValMd' pp) <$> (dictBaseType ct)
       , pure ("size:"           <+> integral (dictSize ct))
       , pure ("align:"          <+> integral (dictAlign ct))
       , pure ("offset:"         <+> integral (dictOffset ct))
       , pure ("flags:"          <+> integral (dictFlags ct))
       ,     (("elements:"       <+>) . ppValMd' pp) <$> (dictElements ct)
       , pure ("runtimeLang:"    <+> integral (dictRuntimeLang ct))
       ,     (("vtableHolder:"   <+>) . ppValMd' pp) <$> (dictVTableHolder ct)
       ,     (("templateParams:" <+>) . ppValMd' pp) <$> (dictTemplateParams ct)
       ,     (("identifier:"     <+>) . doubleQuotes . text)
             <$> (dictIdentifier ct)
       ,     (("discriminator:"  <+>) . ppValMd' pp) <$> (dictDiscriminator ct)
       ,     (("associated:"     <+>) . ppValMd' pp) <$> (dictAssociated ct)
       ,     (("allocated:"      <+>) . ppValMd' pp) <$> (dictAllocated ct)
       ,     (("rank:"           <+>) . ppValMd' pp) <$> (dictRank ct)
       ])

ppDICompositeType :: LLVM => DICompositeType -> Doc
ppDICompositeType = ppDICompositeType' ppLabel

ppDIDerivedType' :: LLVM => (i -> Doc) -> DIDerivedType' i -> Doc
ppDIDerivedType' pp dt = "!DIDerivedType"
  <> parens (mcommas
       [ pure ("tag:"       <+> integral (didtTag dt))
       ,     (("name:"      <+>) . doubleQuotes . text) <$> (didtName dt)
       ,     (("file:"      <+>) . ppValMd' pp) <$> (didtFile dt)
       , pure ("line:"      <+> integral (didtLine dt))
       ,      ("baseType:"  <+>) <$> (ppValMd' pp <$> didtBaseType dt <|> Just "null")
       , pure ("size:"      <+> integral (didtSize dt))
       , pure ("align:"     <+> integral (didtAlign dt))
       , pure ("offset:"    <+> integral (didtOffset dt))
       , pure ("flags:"     <+> integral (didtFlags dt))
       ,     (("extraData:" <+>) . ppValMd' pp) <$> (didtExtraData dt)
       ])

ppDIDerivedType :: LLVM => DIDerivedType -> Doc
ppDIDerivedType = ppDIDerivedType' ppLabel

ppDIEnumerator :: String -> Integer -> Bool -> Doc
ppDIEnumerator n v u = "!DIEnumerator"
  <> parens (commas [ "name:"  <+> doubleQuotes (text n)
                    , "value:" <+> integral v
                    , "isUnsigned:" <+> ppBool u
                    ])

ppDIExpression :: DIExpression -> Doc
ppDIExpression e = "!DIExpression"
  <> parens (commas (map integral (dieElements e)))

ppDIFile :: DIFile -> Doc
ppDIFile f = "!DIFile"
  <> parens (commas [ "filename:"  <+> doubleQuotes (text (difFilename f))
                    , "directory:" <+> doubleQuotes (text (difDirectory f))
                    ])

ppDIGlobalVariable' :: LLVM => (i -> Doc) -> DIGlobalVariable' i -> Doc
ppDIGlobalVariable' pp gv = "!DIGlobalVariable"
  <> parens (mcommas
       [      (("scope:"       <+>) . ppValMd' pp) <$> (digvScope gv)
       ,      (("name:"        <+>) . doubleQuotes . text) <$> (digvName gv)
       ,      (("linkageName:" <+>) . doubleQuotes . text)
              <$> (digvLinkageName gv)
       ,      (("file:"        <+>) . ppValMd' pp) <$> (digvFile gv)
       , pure ("line:"         <+> integral (digvLine gv))
       ,      (("type:"        <+>) . ppValMd' pp) <$> (digvType gv)
       , pure ("isLocal:"      <+> ppBool (digvIsLocal gv))
       , pure ("isDefinition:" <+> ppBool (digvIsDefinition gv))
       ,      (("variable:"    <+>) . ppValMd' pp) <$> (digvVariable gv)
       ,      (("declaration:" <+>) . ppValMd' pp) <$> (digvDeclaration gv)
       ,      (("align:"       <+>) . integral) <$> digvAlignment gv
       ])

ppDIGlobalVariable :: LLVM => DIGlobalVariable -> Doc
ppDIGlobalVariable = ppDIGlobalVariable' ppLabel

ppDIGlobalVariableExpression' :: LLVM => (i -> Doc) -> DIGlobalVariableExpression' i -> Doc
ppDIGlobalVariableExpression' pp gve = "!DIGlobalVariableExpression"
  <> parens (mcommas
       [      (("var:"  <+>) . ppValMd' pp) <$> (digveVariable gve)
       ,      (("expr:" <+>) . ppValMd' pp) <$> (digveExpression gve)
       ])

ppDIGlobalVariableExpression :: LLVM => DIGlobalVariableExpression -> Doc
ppDIGlobalVariableExpression = ppDIGlobalVariableExpression' ppLabel

ppDILexicalBlock' :: LLVM => (i -> Doc) -> DILexicalBlock' i -> Doc
ppDILexicalBlock' pp ct = "!DILexicalBlock"
  <> parens (mcommas
       [     (("scope:"  <+>) . ppValMd' pp) <$> (dilbScope ct)
       ,     (("file:"   <+>) . ppValMd' pp) <$> (dilbFile ct)
       , pure ("line:"   <+> integral (dilbLine ct))
       , pure ("column:" <+> integral (dilbColumn ct))
       ])

ppDILexicalBlock :: LLVM => DILexicalBlock -> Doc
ppDILexicalBlock = ppDILexicalBlock' ppLabel

ppDILexicalBlockFile' :: LLVM => (i -> Doc) -> DILexicalBlockFile' i -> Doc
ppDILexicalBlockFile' pp lbf = "!DILexicalBlockFile"
  <> parens (mcommas
       [ pure ("scope:"         <+> ppValMd' pp (dilbfScope lbf))
       ,     (("file:"          <+>) . ppValMd' pp) <$> (dilbfFile lbf)
       , pure ("discriminator:" <+> integral (dilbfDiscriminator lbf))
       ])

ppDILexicalBlockFile :: LLVM => DILexicalBlockFile -> Doc
ppDILexicalBlockFile = ppDILexicalBlockFile' ppLabel

ppDILocalVariable' :: LLVM => (i -> Doc) -> DILocalVariable' i -> Doc
ppDILocalVariable' pp lv = "!DILocalVariable"
  <> parens (mcommas
       [      (("scope:" <+>) . ppValMd' pp) <$> (dilvScope lv)
       ,      (("name:"  <+>) . doubleQuotes . text) <$> (dilvName lv)
       ,      (("file:"  <+>) . ppValMd' pp) <$> (dilvFile lv)
       , pure ("line:"   <+> integral (dilvLine lv))
       ,      (("type:"  <+>) . ppValMd' pp) <$> (dilvType lv)
       , pure ("arg:"    <+> integral (dilvArg lv))
       , pure ("flags:"  <+> integral (dilvFlags lv))
       ])

ppDILocalVariable :: LLVM => DILocalVariable -> Doc
ppDILocalVariable = ppDILocalVariable' ppLabel

-- | See @writeDISubprogram@ in the LLVM source, in the file @AsmWriter.cpp@
ppDISubprogram' :: LLVM => (i -> Doc) -> DISubprogram' i -> Doc
ppDISubprogram' pp sp = "!DISubprogram"
  <> parens (mcommas
       [      (("scope:"          <+>) . ppValMd' pp) <$> (dispScope sp)
       ,      (("name:"           <+>) . doubleQuotes . text) <$> (dispName sp)
       ,      (("linkageName:"    <+>) . doubleQuotes . text)
              <$> (dispLinkageName sp)
       ,      (("file:"           <+>) . ppValMd' pp) <$> (dispFile sp)
       , pure ("line:"            <+> integral (dispLine sp))
       ,      (("type:"           <+>) . ppValMd' pp) <$> (dispType sp)
       , pure ("isLocal:"         <+> ppBool (dispIsLocal sp))
       , pure ("isDefinition:"    <+> ppBool (dispIsDefinition sp))
       , pure ("scopeLine:"       <+> integral (dispScopeLine sp))
       ,      (("containingType:" <+>) . ppValMd' pp) <$> (dispContainingType sp)
       , pure ("virtuality:"      <+> integral (dispVirtuality sp))
       , pure ("virtualIndex:"    <+> integral (dispVirtualIndex sp))
       , pure ("flags:"           <+> integral (dispFlags sp))
       , pure ("isOptimized:"     <+> ppBool (dispIsOptimized sp))
       ,      (("unit:"           <+>) . ppValMd' pp) <$> (dispUnit sp)
       ,      (("templateParams:" <+>) . ppValMd' pp) <$> (dispTemplateParams sp)
       ,      (("declaration:"    <+>) . ppValMd' pp) <$> (dispDeclaration sp)
       ,      (("variables:"      <+>) . ppValMd' pp) <$> (dispVariables sp)
       ,      (("thrownTypes:"    <+>) . ppValMd' pp) <$> (dispThrownTypes sp)
       ])

ppDISubprogram :: LLVM => DISubprogram -> Doc
ppDISubprogram = ppDISubprogram' ppLabel

ppDISubrange :: DISubrange -> Doc
ppDISubrange sr = "!DISubrange"
  <> parens (commas [ "count:" <+> integral (disrCount sr)
                    , "lowerBound:" <+> integral (disrLowerBound sr)
                    ])

ppDISubroutineType' :: LLVM => (i -> Doc) -> DISubroutineType' i -> Doc
ppDISubroutineType' pp st = "!DISubroutineType"
  <> parens (commas
       [ "flags:" <+> integral (distFlags st)
       , "types:" <+> fromMaybe "null" (ppValMd' pp <$> (distTypeArray st))
       ])

ppDISubroutineType :: LLVM => DISubroutineType -> Doc
ppDISubroutineType = ppDISubroutineType' ppLabel

ppDIArgList' :: LLVM => (i -> Doc) -> DIArgList' i -> Doc
ppDIArgList' pp args = "!DIArgList"
  <> parens (commas (map (ppValMd' pp) (dialArgs args)))

ppDIArgList :: LLVM => DIArgList -> Doc
ppDIArgList = ppDIArgList' ppLabel

-- Utilities -------------------------------------------------------------------

ppBool :: Bool -> Doc
ppBool b | b         = "true"
         | otherwise = "false"

-- | Build a variable-argument argument list.
ppArgList :: Bool -> [Doc] -> Doc
ppArgList True  ds = parens (commas (ds ++ ["..."]))
ppArgList False ds = parens (commas ds)

integral :: Integral i => i -> Doc
integral  = integer . fromIntegral

hex :: (Integral i, Show i) => i -> Doc
hex i = text (showHex i "0x")

opt :: Bool -> Doc -> Doc
opt True  = id
opt False = const empty

commas :: [Doc] -> Doc
commas  = fsep . punctuate comma

-- | Helpful for all of the optional fields that appear in the
-- metadata values
mcommas :: [Maybe Doc] -> Doc
mcommas = commas . catMaybes

angles :: Doc -> Doc
angles d = char '<' <> d <> char '>'

structBraces :: Doc -> Doc
structBraces body = char '{' <+> body <+> char '}'

ppMaybe :: (a -> Doc) -> Maybe a -> Doc
ppMaybe  = maybe empty
