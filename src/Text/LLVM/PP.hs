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

withConfig :: Config -> ((?config :: Config) => a) -> a
withConfig cfg body = let ?config = cfg in body


ppLLVM, ppLLVM35, ppLLVM36, ppLLVM37, ppLLVM38 :: ((?config :: Config) => a) -> a

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

checkConfig :: (?config :: Config) => (Config -> Bool) -> Bool
checkConfig p = p ?config


type Fmt a = (?config :: Config) => a -> Doc


-- | The LLVMPretty class has instances for most AST elements.  It allows the
-- conversion of an AST element (and its sub-elements) into a Doc assembly format
-- by simply using the 'llvmPP' method rather than needing to explicitly invoke
-- the specific pretty-printing function for that element.
class LLVMPretty a where llvmPP :: Fmt a

instance LLVMPretty Module where llvmPP = ppModule
instance LLVMPretty Symbol where llvmPP = ppSymbol


-- Modules ---------------------------------------------------------------------

ppModule :: Fmt Module
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

ppSourceName :: Fmt (Maybe String)
ppSourceName Nothing   = empty
ppSourceName (Just sn) = "source_filename" <+> char '=' <+> doubleQuotes (text sn)

-- Metadata --------------------------------------------------------------------

ppNamedMd :: Fmt NamedMd
ppNamedMd nm =
  sep [ ppMetadata (text (nmName nm)) <+> char '='
      , ppMetadata (braces (commas (map (ppMetadata . int) (nmValues nm)))) ]

ppUnnamedMd :: Fmt UnnamedMd
ppUnnamedMd um =
  sep [ ppMetadata (int (umIndex um)) <+> char '='
      , distinct <+> ppValMd (umValues um) ]
  where
  distinct | umDistinct um = "distinct"
           | otherwise     = empty


-- Aliases ---------------------------------------------------------------------

ppGlobalAlias :: Fmt GlobalAlias
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
ppTargetTriple :: Fmt TargetTriple
ppTargetTriple triple = "target" <+> "triple" <+> char '='
    <+> doubleQuotes (text (printTriple triple))

-- Data Layout -----------------------------------------------------------------

-- | Pretty print a data layout specification.
ppDataLayout :: Fmt DataLayout
ppDataLayout [] = empty
ppDataLayout ls = "target" <+> "datalayout" <+> char '='
    <+> doubleQuotes (hcat (intersperse (char '-') (map ppLayoutSpec ls)))

-- | Pretty print a single layout specification.
ppLayoutSpec :: Fmt LayoutSpec
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
ppLayoutBody :: Int -> Int -> Fmt (Maybe Int)
ppLayoutBody size abi mb = int size <> char ':' <> int abi <> pref
  where
  pref = case mb of
    Nothing -> empty
    Just p  -> char ':' <> int p

ppMangling :: Fmt Mangling
ppMangling ElfMangling         = char 'e'
ppMangling MipsMangling        = char 'm'
ppMangling MachOMangling       = char 'o'
ppMangling WindowsCoffMangling = char 'w'


-- Inline Assembly -------------------------------------------------------------

-- | Pretty-print the inline assembly block.
ppInlineAsm :: Fmt InlineAsm
ppInlineAsm  = foldr ($+$) empty . map ppLine
  where
  ppLine l = "module asm" <+> doubleQuotes (text l)


-- Identifiers -----------------------------------------------------------------

ppIdent :: Fmt Ident
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

ppSymbol :: Fmt Symbol
ppSymbol (Symbol n)
  | validIdentifier n = char '@' <> text n
  | otherwise         = char '@' <> ppStringLiteral n


-- Types -----------------------------------------------------------------------

ppPrimType :: Fmt PrimType
ppPrimType Label          = "label"
ppPrimType Void           = "void"
ppPrimType (Integer i)    = char 'i' <> integer (toInteger i)
ppPrimType (FloatType ft) = ppFloatType ft
ppPrimType X86mmx         = "x86mmx"
ppPrimType Metadata       = "metadata"

ppFloatType :: Fmt FloatType
ppFloatType Half      = "half"
ppFloatType Float     = "float"
ppFloatType Double    = "double"
ppFloatType Fp128     = "fp128"
ppFloatType X86_fp80  = "x86_fp80"
ppFloatType PPC_fp128 = "ppc_fp128"

ppType :: Fmt Type
ppType (PrimType pt)     = ppPrimType pt
ppType (Alias i)         = ppIdent i
ppType (Array len ty)    = brackets (integral len <+> char 'x' <+> ppType ty)
ppType (PtrTo ty)        = ppType ty <> char '*'
ppType PtrOpaque         = "ptr"
ppType (Struct ts)       = structBraces (commas (map ppType ts))
ppType (PackedStruct ts) = angles (structBraces (commas (map ppType ts)))
ppType (FunTy r as va)   = ppType r <> ppArgList va (map ppType as)
ppType (Vector len pt)   = angles (integral len <+> char 'x' <+> ppType pt)
ppType Opaque            = "opaque"

ppTypeDecl :: Fmt TypeDecl
ppTypeDecl td = ppIdent (typeName td) <+> char '='
            <+> "type" <+> ppType (typeValue td)


-- Declarations ----------------------------------------------------------------

ppGlobal :: Fmt Global
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

ppGlobalAttrs :: Fmt GlobalAttrs
ppGlobalAttrs ga
    -- LLVM 3.8 does not emit or parse linkage information w/ hidden visibility
    | Just HiddenVisibility <- gaVisibility ga =
            ppVisibility HiddenVisibility <+> constant
    | otherwise = ppMaybe ppLinkage (gaLinkage ga) <+> ppMaybe ppVisibility (gaVisibility ga) <+> constant
  where
  constant | gaConstant ga = "constant"
           | otherwise     = "global"

ppStructGlobalAttrs :: Fmt GlobalAttrs
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

ppDeclare :: Fmt Declare
ppDeclare d = "declare"
          <+> ppMaybe ppLinkage (decLinkage d)
          <+> ppMaybe ppVisibility (decVisibility d)
          <+> ppType (decRetType d)
          <+> ppSymbol (decName d)
           <> ppArgList (decVarArgs d) (map ppType (decArgs d))
          <+> hsep (ppFunAttr <$> decAttrs d)
          <> maybe empty ((char ' ' <>) . ppComdatName) (decComdat d)

ppComdatName :: Fmt String
ppComdatName s = "comdat" <> parens (char '$' <> text s)

ppComdat :: Fmt (String,SelectionKind)
ppComdat (n,k) = ppComdatName n <+> char '=' <+> text "comdat" <+> ppSelectionKind k

ppSelectionKind :: Fmt SelectionKind
ppSelectionKind k =
    case k of
      ComdatAny             -> "any"
      ComdatExactMatch      -> "exactmatch"
      ComdatLargest         -> "largest"
      ComdatNoDuplicates    -> "noduplicates"
      ComdatSameSize        -> "samesize"

ppDefine :: Fmt Define
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

ppFunAttr :: Fmt FunAttr
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

ppLabelDef :: Fmt BlockLabel
ppLabelDef (Named (Ident l)) = text l <> char ':'
ppLabelDef (Anon i)          = char ';' <+> "<label>:" <+> int i

ppLabel :: Fmt BlockLabel
ppLabel (Named l) = ppIdent l
ppLabel (Anon i)  = char '%' <> int i

ppBasicBlock :: Fmt BasicBlock
ppBasicBlock bb = ppMaybe ppLabelDef (bbLabel bb)
              $+$ nest 2 (vcat (map ppStmt (bbStmts bb)))


-- Statements ------------------------------------------------------------------

ppStmt :: Fmt Stmt
ppStmt stmt = case stmt of
  Result var i mds -> ppIdent var <+> char '=' <+> ppInstr i
                   <> ppAttachedMetadata mds
  Effect i mds     -> ppInstr i <> ppAttachedMetadata mds

ppAttachedMetadata :: Fmt [(String,ValMd)]
ppAttachedMetadata mds
  | null mds  = empty
  | otherwise = comma <+> commas (map step mds)
  where
  step (l,md) = ppMetadata (text l) <+> ppValMd md


-- Linkage ---------------------------------------------------------------------

ppLinkage :: Fmt Linkage
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

ppVisibility :: Fmt Visibility
ppVisibility v = case v of
    DefaultVisibility   -> "default"
    HiddenVisibility    -> "hidden"
    ProtectedVisibility -> "protected"

ppGC :: Fmt GC
ppGC  = doubleQuotes . text . getGC


-- Expressions -----------------------------------------------------------------

ppTyped :: Fmt a -> Fmt (Typed a)
ppTyped fmt ty = ppType (typedType ty) <+> fmt (typedValue ty)

ppSignBits :: Bool -> Fmt Bool
ppSignBits nuw nsw = opt nuw "nuw" <+> opt nsw "nsw"

ppExact :: Fmt Bool
ppExact e = opt e "exact"

ppArithOp :: Fmt ArithOp
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

ppUnaryArithOp :: Fmt UnaryArithOp
ppUnaryArithOp FNeg = "fneg"

ppBitOp :: Fmt BitOp
ppBitOp (Shl nuw nsw) = "shl"  <+> ppSignBits nuw nsw
ppBitOp (Lshr e)      = "lshr" <+> ppExact e
ppBitOp (Ashr e)      = "ashr" <+> ppExact e
ppBitOp And           = "and"
ppBitOp Or            = "or"
ppBitOp Xor           = "xor"

ppConvOp :: Fmt ConvOp
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

ppAtomicOrdering :: Fmt AtomicOrdering
ppAtomicOrdering Unordered = text "unordered"
ppAtomicOrdering Monotonic = text "monotonic"
ppAtomicOrdering Acquire   = text "acquire"
ppAtomicOrdering Release   = text "release"
ppAtomicOrdering AcqRel    = text "acq_rel"
ppAtomicOrdering SeqCst    = text "seq_cst"

ppAtomicOp :: Fmt AtomicRWOp
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

ppScope ::  Fmt (Maybe String)
ppScope Nothing = empty
ppScope (Just s) = "syncscope" <> parens (doubleQuotes (text s))

ppInstr :: Fmt Instr
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
  Load ty ptr mo ma      -> ppLoad ty ptr mo ma
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
  GEP ib ty ptr ixs      -> ppGEP ib ty ptr ixs
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

ppLoad :: Type -> Typed (Value' BlockLabel) -> Maybe AtomicOrdering -> Fmt (Maybe Align)
ppLoad ty ptr mo ma =
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

  explicit = ppType ty <> comma

ppStore :: Typed (Value' BlockLabel)
        -> Typed (Value' BlockLabel)
        -> Maybe AtomicOrdering
        -> Fmt (Maybe Align)
ppStore ptr val mo ma =
  "store" <+> (if isJust mo  then "atomic" else empty)
          <+> ppTyped ppValue ptr <> comma
          <+> ppTyped ppValue val
          <+> case mo of
                Just ao -> ppAtomicOrdering ao
                _       -> empty
          <> ppAlign ma


ppClauses :: Bool -> Fmt [Clause]
ppClauses isCleanup cs = vcat (cleanup : map ppClause cs)
  where
  cleanup | isCleanup = "cleanup"
          | otherwise = empty

ppClause :: Fmt Clause
ppClause c = case c of
  Catch  tv -> "catch"  <+> ppTyped ppValue tv
  Filter tv -> "filter" <+> ppTyped ppValue tv


ppTypedLabel :: Fmt BlockLabel
ppTypedLabel i = ppType (PrimType Label) <+> ppLabel i

ppSwitchEntry :: Type -> Fmt (Integer,BlockLabel)
ppSwitchEntry ty (i,l) = ppType ty <+> integer i <> comma <+> ppTypedLabel l

ppVectorIndex :: Fmt Value
ppVectorIndex i = ppType (PrimType (Integer 32)) <+> ppValue i

ppAlign :: Fmt (Maybe Align)
ppAlign Nothing      = empty
ppAlign (Just align) = comma <+> "align" <+> int align

ppAlloca :: Type -> Maybe (Typed Value) -> Fmt (Maybe Int)
ppAlloca ty mbLen mbAlign = "alloca" <+> ppType ty <> len <> align
  where
  len = fromMaybe empty $ do
    l <- mbLen
    return (comma <+> ppTyped ppValue l)
  align = fromMaybe empty $ do
    a <- mbAlign
    return (comma <+> "align" <+> int a)

ppCall :: Bool -> Type -> Value -> Fmt [Typed Value]
ppCall tc ty f args
  | tc        = "tail" <+> body
  | otherwise = body
  where
  body = "call" <+> ppCallSym ty f
      <> parens (commas (map (ppTyped ppValue) args))

-- | Note that the textual syntax changed in LLVM 10 (@callbr@ was introduced in
-- LLVM 9).
ppCallBr :: Type -> Value -> [Typed Value] -> BlockLabel -> Fmt [BlockLabel]
ppCallBr ty f args to indirectDests =
  "callbr"
     <+> ppCallSym ty f <> parens (commas (map (ppTyped ppValue) args))
     <+> "to" <+> ppLab to <+> brackets (commas (map ppLab indirectDests))
  where
    ppLab l = ppType (PrimType Label) <+> ppLabel l

-- | Print out the @<ty>|<fnty> <fnptrval>@ portion of a @call@, @callbr@, or
-- @invoke@ instruction, where:
--
-- * @<ty>@ is the return type.
--
-- * @<fnty>@ is the overall function type.
--
-- * @<fnptrval>@ is a pointer value, where the memory it points to is treated
--   as a value of type @<fnty>@.
--
-- The LLVM Language Reference Manual indicates that either @<ty>@ or @<fnty>@
-- can be used, but in practice, @<ty>@ is typically preferred unless the
-- function type involves varargs. We adopt the same convention here.
ppCallSym :: Type -> Fmt Value
ppCallSym ty val = pp_ty <+> ppValue val
  where
    pp_ty =
      case ty of
        FunTy res args va
          |  va
          -> ppType res <+> ppArgList va (map ppType args)
          |  otherwise
          -> ppType res
        _ -> ppType ty

ppGEP :: Bool -> Type -> Typed Value -> Fmt [Typed Value]
ppGEP ib ty ptr ixs =
  "getelementptr" <+> inbounds
    <+> (if isImplicit then empty else explicit)
    <+> commas (map (ppTyped ppValue) (ptr:ixs))
  where
  isImplicit = checkConfig cfgGEPImplicitType

  explicit = ppType ty <> comma

  inbounds | ib        = "inbounds"
           | otherwise = empty

ppInvoke :: Type -> Value -> [Typed Value] -> BlockLabel -> Fmt BlockLabel
ppInvoke ty f args to uw = body
  where
  body = "invoke" <+> ppCallSym ty f
      <> parens (commas (map (ppTyped ppValue) args))
     <+> "to" <+> ppType (PrimType Label) <+> ppLabel to
     <+> "unwind" <+> ppType (PrimType Label) <+> ppLabel uw

ppPhiArg :: Fmt (Value,BlockLabel)
ppPhiArg (v,l) = char '[' <+> ppValue v <> comma <+> ppLabel l <+> char ']'

ppICmpOp :: Fmt ICmpOp
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

ppFCmpOp :: Fmt FCmpOp
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

ppValue' :: Fmt i -> Fmt (Value' i)
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

ppValue :: Fmt Value
ppValue = ppValue' ppLabel

ppValMd' :: Fmt i -> Fmt (ValMd' i)
ppValMd' pp m = case m of
  ValMdString str   -> ppMetadata (ppStringLiteral str)
  ValMdValue tv     -> ppTyped (ppValue' pp) tv
  ValMdRef i        -> ppMetadata (int i)
  ValMdNode vs      -> ppMetadataNode' pp vs
  ValMdLoc l        -> ppDebugLoc' pp l
  ValMdDebugInfo di -> ppDebugInfo' pp di

ppValMd :: Fmt ValMd
ppValMd = ppValMd' ppLabel

ppDebugLoc' :: Fmt i -> Fmt (DebugLoc' i)
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

ppDebugLoc :: Fmt DebugLoc
ppDebugLoc = ppDebugLoc' ppLabel

ppTypedValMd :: Fmt ValMd
ppTypedValMd  = ppTyped ppValMd . Typed (PrimType Metadata)

ppMetadata :: Fmt Doc
ppMetadata body = char '!' <> body

ppMetadataNode' :: Fmt i -> Fmt [Maybe (ValMd' i)]
ppMetadataNode' pp vs = ppMetadata (braces (commas (map arg vs)))
  where arg = maybe ("null") (ppValMd' pp)

ppMetadataNode :: Fmt [Maybe ValMd]
ppMetadataNode = ppMetadataNode' ppLabel

ppStringLiteral :: Fmt String
ppStringLiteral  = doubleQuotes . text . concatMap escape
  where
  escape c | c == '"' || c == '\\'  = '\\' : showHex (fromEnum c) ""
           | isAscii c && isPrint c = [c]
           | otherwise              = '\\' : pad (ord c)

  pad n | n < 0x10  = '0' : map toUpper (showHex n "")
        | otherwise =       map toUpper (showHex n "")

ppAsm :: Bool -> Bool -> String -> Fmt String
ppAsm s a i c =
  "asm" <+> sideeffect <+> alignstack
        <+> ppStringLiteral i <> comma <+> ppStringLiteral c
  where
  sideeffect | s         = "sideeffect"
             | otherwise = empty

  alignstack | a         = "alignstack"
             | otherwise = empty


ppConstExpr' :: Fmt i -> Fmt (ConstExpr' i)
ppConstExpr' pp expr =
  case expr of
    ConstGEP inb _mix ty ptr ixs  ->
      "getelementptr"
        <+> opt inb "inbounds"
        <+> parens (commas (ppType ty : map ppTyp' (ptr:ixs)))
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

ppConstExpr :: Fmt ConstExpr
ppConstExpr = ppConstExpr' ppLabel

-- DWARF Debug Info ------------------------------------------------------------

ppDebugInfo' :: Fmt i -> Fmt (DebugInfo' i)
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

ppDebugInfo :: Fmt DebugInfo
ppDebugInfo = ppDebugInfo' ppLabel

ppDIImportedEntity' :: Fmt i -> Fmt (DIImportedEntity' i)
ppDIImportedEntity' pp ie = "!DIImportedEntity"
  <> parens (mcommas [ pure ("tag:"    <+> integral (diieTag ie))
                     , (("scope:"  <+>) . ppValMd' pp) <$> diieScope ie
                     , (("entity:" <+>) . ppValMd' pp) <$> diieEntity ie
                     , (("file:"   <+>) . ppValMd' pp) <$> diieFile ie
                     , pure ("line:"   <+> integral (diieLine ie))
                     , (("name:"   <+>) . text)        <$> diieName ie
                     ])

ppDIImportedEntity :: Fmt DIImportedEntity
ppDIImportedEntity = ppDIImportedEntity' ppLabel

ppDILabel' :: Fmt i -> Fmt (DILabel' i)
ppDILabel' pp ie = "!DILabel"
  <> parens (mcommas [ (("scope:"  <+>) . ppValMd' pp) <$> dilScope ie
                     , pure ("name:" <+> text (dilName ie))
                     , (("file:"   <+>) . ppValMd' pp) <$> dilFile ie
                     , pure ("line:"   <+> integral (dilLine ie))
                     ])

ppDILabel :: Fmt DILabel
ppDILabel = ppDILabel' ppLabel

ppDINameSpace' :: Fmt i -> Fmt (DINameSpace' i)
ppDINameSpace' pp ns = "!DINameSpace"
  <> parens (mcommas [ ("name:"   <+>) . text <$> (dinsName ns)
                     , pure ("scope:"  <+> ppValMd' pp (dinsScope ns))
                     , pure ("file:"   <+> ppValMd' pp (dinsFile ns))
                     , pure ("line:"   <+> integral (dinsLine ns))
                     ])

ppDINameSpace :: Fmt DINameSpace
ppDINameSpace = ppDINameSpace' ppLabel

ppDITemplateTypeParameter' :: Fmt i -> Fmt (DITemplateTypeParameter' i)
ppDITemplateTypeParameter' pp tp = "!DITemplateTypeParameter"
  <> parens (mcommas [ ("name:"  <+>) . text        <$> dittpName tp
                     , ("type:"  <+>) . ppValMd' pp <$> dittpType tp
                     ])

ppDITemplateTypeParameter :: Fmt DITemplateTypeParameter
ppDITemplateTypeParameter = ppDITemplateTypeParameter' ppLabel

ppDITemplateValueParameter' :: Fmt i -> Fmt (DITemplateValueParameter' i)
ppDITemplateValueParameter' pp vp = "!DITemplateValueParameter"
  <> parens (mcommas [ pure ("tag:"   <+> integral (ditvpTag vp))
                     , ("name:"  <+>) . text        <$> ditvpName vp
                     , ("type:"  <+>) . ppValMd' pp <$> ditvpType vp
                     , pure ("value:" <+> ppValMd' pp (ditvpValue vp))
                     ])

ppDITemplateValueParameter :: Fmt DITemplateValueParameter
ppDITemplateValueParameter = ppDITemplateValueParameter' ppLabel

ppDIBasicType :: Fmt DIBasicType
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

ppDICompileUnit' :: Fmt i -> Fmt (DICompileUnit' i)
ppDICompileUnit' pp cu = "!DICompileUnit"
  <> parens (mcommas
       [ pure ("language:"              <+> integral (dicuLanguage cu))
       ,     (("file:"                  <+>) . ppValMd' pp) <$> (dicuFile cu)
       ,     (("producer:"              <+>) . doubleQuotes . text)
             <$> (dicuProducer cu)
       , pure ("isOptimized:"           <+> ppBool (dicuIsOptimized cu))
       , pure ("flags:"                 <+> ppFlags (dicuFlags cu))
       , pure ("runtimeVersion:"        <+> integral (dicuRuntimeVersion cu))
       ,     (("splitDebugFilename:"    <+>) . doubleQuotes . text)
             <$> (dicuSplitDebugFilename cu)
       , pure ("emissionKind:"          <+> integral (dicuEmissionKind cu))
       ,     (("enums:"                 <+>) . ppValMd' pp) <$> (dicuEnums cu)
       ,     (("retainedTypes:"         <+>) . ppValMd' pp) <$> (dicuRetainedTypes cu)
       ,     (("subprograms:"           <+>) . ppValMd' pp) <$> (dicuSubprograms cu)
       ,     (("globals:"               <+>) . ppValMd' pp) <$> (dicuGlobals cu)
       ,     (("imports:"               <+>) . ppValMd' pp) <$> (dicuImports cu)
       ,     (("macros:"                <+>) . ppValMd' pp) <$> (dicuMacros cu)
       , pure ("dwoId:"                 <+> integral (dicuDWOId cu))
       , pure ("splitDebugInlining:"    <+> ppBool (dicuSplitDebugInlining cu))
       , pure ("debugInfoForProfiling:" <+> ppBool (dicuDebugInfoForProf cu))
       , pure ("nameTableKind:"         <+> integral (dicuNameTableKind cu))
       , pure ("rangesBaseAddress:"     <+> ppBool (dicuRangesBaseAddress cu))
       ,     (("sysroot:"               <+>) . doubleQuotes . text)
             <$> (dicuSysRoot cu)
       ,     (("sdk:"                   <+>) . doubleQuotes . text)
             <$> (dicuSDK cu)
       ])

ppDICompileUnit :: Fmt DICompileUnit
ppDICompileUnit = ppDICompileUnit' ppLabel

ppFlags :: Fmt (Maybe String)
ppFlags mb = doubleQuotes (maybe empty text mb)

ppDICompositeType' :: Fmt i -> Fmt (DICompositeType' i)
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
       ,     (("annotations:"    <+>) . ppValMd' pp) <$> (dictAnnotations ct)
       ])

ppDICompositeType :: Fmt DICompositeType
ppDICompositeType = ppDICompositeType' ppLabel

ppDIDerivedType' :: Fmt i -> Fmt (DIDerivedType' i)
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
       ,     (("dwarfAddressSpace:" <+>) . integral) <$> didtDwarfAddressSpace dt
       ,     (("annotations:" <+>) . ppValMd' pp) <$> (didtAnnotations dt)
       ])

ppDIDerivedType :: Fmt DIDerivedType
ppDIDerivedType = ppDIDerivedType' ppLabel

ppDIEnumerator :: String -> Integer -> Fmt Bool
ppDIEnumerator n v u = "!DIEnumerator"
  <> parens (commas [ "name:"  <+> doubleQuotes (text n)
                    , "value:" <+> integral v
                    , "isUnsigned:" <+> ppBool u
                    ])

ppDIExpression :: Fmt DIExpression
ppDIExpression e = "!DIExpression"
  <> parens (commas (map integral (dieElements e)))

ppDIFile :: Fmt DIFile
ppDIFile f = "!DIFile"
  <> parens (commas [ "filename:"  <+> doubleQuotes (text (difFilename f))
                    , "directory:" <+> doubleQuotes (text (difDirectory f))
                    ])

ppDIGlobalVariable' :: Fmt i -> Fmt (DIGlobalVariable' i)
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
       ,      (("annotations:" <+>) . ppValMd' pp) <$> (digvAnnotations gv)
       ])

ppDIGlobalVariable :: Fmt DIGlobalVariable
ppDIGlobalVariable = ppDIGlobalVariable' ppLabel

ppDIGlobalVariableExpression' :: Fmt i -> Fmt (DIGlobalVariableExpression' i)
ppDIGlobalVariableExpression' pp gve = "!DIGlobalVariableExpression"
  <> parens (mcommas
       [      (("var:"  <+>) . ppValMd' pp) <$> (digveVariable gve)
       ,      (("expr:" <+>) . ppValMd' pp) <$> (digveExpression gve)
       ])

ppDIGlobalVariableExpression :: Fmt DIGlobalVariableExpression
ppDIGlobalVariableExpression = ppDIGlobalVariableExpression' ppLabel

ppDILexicalBlock' :: Fmt i -> Fmt (DILexicalBlock' i)
ppDILexicalBlock' pp ct = "!DILexicalBlock"
  <> parens (mcommas
       [     (("scope:"  <+>) . ppValMd' pp) <$> (dilbScope ct)
       ,     (("file:"   <+>) . ppValMd' pp) <$> (dilbFile ct)
       , pure ("line:"   <+> integral (dilbLine ct))
       , pure ("column:" <+> integral (dilbColumn ct))
       ])

ppDILexicalBlock :: Fmt DILexicalBlock
ppDILexicalBlock = ppDILexicalBlock' ppLabel

ppDILexicalBlockFile' :: Fmt i -> Fmt (DILexicalBlockFile' i)
ppDILexicalBlockFile' pp lbf = "!DILexicalBlockFile"
  <> parens (mcommas
       [ pure ("scope:"         <+> ppValMd' pp (dilbfScope lbf))
       ,     (("file:"          <+>) . ppValMd' pp) <$> (dilbfFile lbf)
       , pure ("discriminator:" <+> integral (dilbfDiscriminator lbf))
       ])

ppDILexicalBlockFile :: Fmt DILexicalBlockFile
ppDILexicalBlockFile = ppDILexicalBlockFile' ppLabel

ppDILocalVariable' :: Fmt i -> Fmt (DILocalVariable' i)
ppDILocalVariable' pp lv = "!DILocalVariable"
  <> parens (mcommas
       [      (("scope:" <+>) . ppValMd' pp) <$> (dilvScope lv)
       ,      (("name:"  <+>) . doubleQuotes . text) <$> (dilvName lv)
       ,      (("file:"  <+>) . ppValMd' pp) <$> (dilvFile lv)
       , pure ("line:"   <+> integral (dilvLine lv))
       ,      (("type:"  <+>) . ppValMd' pp) <$> (dilvType lv)
       , pure ("arg:"    <+> integral (dilvArg lv))
       , pure ("flags:"  <+> integral (dilvFlags lv))
       ,      (("align:" <+>) . integral) <$> dilvAlignment lv
       ,      (("annotations:" <+>) . ppValMd' pp) <$> (dilvAnnotations lv)
       ])

ppDILocalVariable :: Fmt DILocalVariable
ppDILocalVariable = ppDILocalVariable' ppLabel

-- | See @writeDISubprogram@ in the LLVM source, in the file @AsmWriter.cpp@
--
-- Note that the textual syntax changed in LLVM 7, as the @retainedNodes@ field
-- was called @variables@ in previous LLVM versions.
ppDISubprogram' :: Fmt i -> Fmt (DISubprogram' i)
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
       ,      (("retainedNodes:"  <+>) . ppValMd' pp) <$> (dispRetainedNodes sp)
       ,      (("thrownTypes:"    <+>) . ppValMd' pp) <$> (dispThrownTypes sp)
       ,      (("annotations:"    <+>) . ppValMd' pp) <$> (dispAnnotations sp)
       ])

ppDISubprogram :: Fmt DISubprogram
ppDISubprogram = ppDISubprogram' ppLabel

ppDISubrange :: Fmt DISubrange
ppDISubrange sr = "!DISubrange"
  <> parens (commas [ "count:" <+> integral (disrCount sr)
                    , "lowerBound:" <+> integral (disrLowerBound sr)
                    ])

ppDISubroutineType' :: Fmt i -> Fmt (DISubroutineType' i)
ppDISubroutineType' pp st = "!DISubroutineType"
  <> parens (commas
       [ "flags:" <+> integral (distFlags st)
       , "types:" <+> fromMaybe "null" (ppValMd' pp <$> (distTypeArray st))
       ])

ppDISubroutineType :: Fmt DISubroutineType
ppDISubroutineType = ppDISubroutineType' ppLabel

ppDIArgList' :: Fmt i -> Fmt (DIArgList' i)
ppDIArgList' pp args = "!DIArgList"
  <> parens (commas (map (ppValMd' pp) (dialArgs args)))

ppDIArgList :: Fmt DIArgList
ppDIArgList = ppDIArgList' ppLabel

-- Utilities -------------------------------------------------------------------

ppBool :: Fmt Bool
ppBool b | b         = "true"
         | otherwise = "false"

-- | Build a variable-argument argument list.
ppArgList :: Bool -> Fmt [Doc]
ppArgList True  ds = parens (commas (ds ++ ["..."]))
ppArgList False ds = parens (commas ds)

integral :: Integral i => Fmt i
integral  = integer . fromIntegral

hex :: (Integral i, Show i) => Fmt i
hex i = text (showHex i "0x")

opt :: Bool -> Fmt Doc
opt True  = id
opt False = const empty

commas :: Fmt [Doc]
commas  = fsep . punctuate comma

-- | Helpful for all of the optional fields that appear in the
-- metadata values
mcommas :: Fmt [Maybe Doc]
mcommas = commas . catMaybes

angles :: Fmt Doc
angles d = char '<' <> d <> char '>'

structBraces :: Fmt Doc
structBraces body = char '{' <+> body <+> char '}'

ppMaybe :: Fmt a -> Fmt (Maybe a)
ppMaybe  = maybe empty
