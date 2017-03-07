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

import Data.Char (isAscii,isPrint,ord,toUpper)
import Data.List (intersperse)
import qualified Data.Map as Map
import Data.Maybe (catMaybes,fromMaybe)
import Numeric (showHex)
import Text.PrettyPrint.HughesPJ
import Data.Int


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
                     }

withConfig :: Config -> (LLVM => a) -> a
withConfig cfg body = let ?config = cfg in body


ppLLVM, ppLLVM35, ppLLVM36, ppLLVM37, ppLLVM38 :: (LLVM => a) -> a

ppLLVM = ppLLVM38

ppLLVM35 = ppLLVM36

ppLLVM36 = withConfig Config { cfgLoadImplicitType = True
                             , cfgGEPImplicitType  = True
                             }
ppLLVM37 = withConfig Config { cfgLoadImplicitType = False
                             , cfgGEPImplicitType  = False
                             }
ppLLVM38 = withConfig Config { cfgLoadImplicitType = False
                             , cfgGEPImplicitType  = False
                             }

checkConfig :: LLVM => (Config -> Bool) -> Bool
checkConfig p = p ?config


-- Modules ---------------------------------------------------------------------

ppModule :: LLVM => Module -> Doc
ppModule m = foldr ($+$) empty
  $ ppDataLayout (modDataLayout m)
  : ppInlineAsm  (modInlineAsm m)
  : concat [ map ppTypeDecl    (modTypes m)
           , map ppGlobal      (modGlobals m)
           , map ppGlobalAlias (modAliases m)
           , map ppDeclare     (modDeclares m)
           , map ppDefine      (modDefines m)
           , map ppNamedMd     (modNamedMd m)
           , map ppUnnamedMd   (modUnnamedMd m)
           ]


-- Metadata --------------------------------------------------------------------

ppNamedMd :: NamedMd -> Doc
ppNamedMd nm =
  sep [ ppMetadata (text (nmName nm)) <+> char '='
      , ppMetadata (braces (commas (map (ppMetadata . int) (nmValues nm)))) ]

ppUnnamedMd :: UnnamedMd -> Doc
ppUnnamedMd um =
  sep [ ppMetadata (int (umIndex um)) <+> char '='
      , distinct <+> ppValMd (umValues um) ]
  where
  distinct | umDistinct um = "distinct"
           | otherwise     = empty


-- Aliases ---------------------------------------------------------------------

ppGlobalAlias :: GlobalAlias -> Doc
ppGlobalAlias g = ppSymbol (aliasName g) <+> char '=' <+> body
  where
  val  = aliasTarget g
  body = case val of
    ValSymbol _sym -> ppType (aliasType g) <+> ppValue val
    _              -> ppValue val


-- Data Layout -----------------------------------------------------------------

-- | Pretty print a data layout specification.
ppDataLayout :: DataLayout -> Doc
ppDataLayout [] = empty
ppDataLayout ls = "target" <+> "datalayout" <+> char '='
    <+> doubleQuotes (hcat (intersperse (char '-') (map ppLayoutSpec ls)))

-- | Pretty print a single layout specification.
ppLayoutSpec :: LayoutSpec -> Doc
ppLayoutSpec  BigEndian                  = char 'E'
ppLayoutSpec  LittleEndian               = char 'e'
ppLayoutSpec (PointerSize   sz abi pref) =      "p:" <> ppLayoutBody sz abi pref
ppLayoutSpec (IntegerSize   sz abi pref) = char 'i'  <> ppLayoutBody sz abi pref
ppLayoutSpec (VectorSize    sz abi pref) = char 'v'  <> ppLayoutBody sz abi pref
ppLayoutSpec (FloatSize     sz abi pref) = char 'f'  <> ppLayoutBody sz abi pref
ppLayoutSpec (AggregateSize sz abi pref) = char 'a'  <> ppLayoutBody sz abi pref
ppLayoutSpec (StackObjSize  sz abi pref) = char 's'  <> ppLayoutBody sz abi pref
ppLayoutSpec (NativeIntSize szs)         =
  char 'n' <> hcat (punctuate (char ':') (map int szs))
ppLayoutSpec (StackAlign a)              = char 'S'  <> int a
ppLayoutSpec (Mangling m)                = char 'm'  <> char ':' <> ppMangling m

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
ppIdent (Ident n) = char '%' <> text n


-- Symbols ---------------------------------------------------------------------

ppSymbol :: Symbol -> Doc
ppSymbol (Symbol n) = char '@' <> text n


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

ppGlobal :: Global -> Doc
ppGlobal g = ppSymbol (globalSym g) <+> char '='
         <+> ppGlobalAttrs (globalAttrs g)
         <+> ppType (globalType g) <+> ppMaybe ppValue (globalValue g)
          <> ppAlign (globalAlign g)

ppGlobalAttrs :: GlobalAttrs -> Doc
ppGlobalAttrs ga = ppMaybe ppLinkage (gaLinkage ga) <+> constant
  where
  constant | gaConstant ga = "constant"
           | otherwise     = "global"


ppDeclare :: Declare -> Doc
ppDeclare d = "declare"
          <+> ppType (decRetType d)
          <+> ppSymbol (decName d)
           <> ppArgList (decVarArgs d) (map ppType (decArgs d))
          <+> hsep (ppFunAttr <$> decAttrs d)



ppDefine :: LLVM => Define -> Doc
ppDefine d = "define"
         <+> ppMaybe ppLinkage (defLinkage d)
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

ppAttachedMetadata :: [(String,ValMd)] -> Doc
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

ppInstr :: LLVM => Instr -> Doc
ppInstr instr = case instr of
  Ret tv                 -> "ret" <+> ppTyped ppValue tv
  RetVoid                -> "ret void"
  Arith op l r           -> ppArithOp op <+> ppTyped ppValue l
                         <> comma <+> ppValue r
  Bit op l r             -> ppBitOp op <+> ppTyped ppValue l
                         <> comma <+> ppValue r
  Conv op a ty           -> ppConvOp op <+> ppTyped ppValue a
                        <+> "to" <+> ppType ty
  Call tc ty f args      -> ppCall tc ty f args
  Alloca ty len align    -> ppAlloca ty len align
  Load ptr ma            -> ppLoad ptr ma
  Store a ptr ma         -> "store" <+> ppTyped ppValue a
                         <> comma <+> ppTyped ppValue ptr
                         <> ppAlign ma
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
  LandingPad ty fn c cs  -> "landingpad"
                        <+> ppType ty
                        <+> "personality"
                        <+> ppTyped ppValue fn
                         $$ nest 2 (ppClauses c cs)
  Resume tv              -> "resume" <+> ppTyped ppValue tv

ppLoad :: LLVM => Typed (Value' BlockLabel) -> Maybe Align -> Doc
ppLoad ptr ma =
  "load" <+> (if isImplicit then empty else explicit)
         <+> ppTyped ppValue ptr
          <> ppAlign ma

  where
  isImplicit = checkConfig cfgLoadImplicitType

  explicit =
    case typedType ptr of
      PtrTo ty -> ppType ty <> comma
      ty       -> ppType ty <> comma

ppClauses :: Bool -> [Clause] -> Doc
ppClauses isCleanup cs = vcat (cleanup : map ppClause cs)
  where
  cleanup | isCleanup = "cleanup"
          | otherwise = empty

ppClause :: Clause -> Doc
ppClause c = case c of
  Catch  tv -> "catch"  <+> ppTyped ppValue tv
  Filter tv -> "filter" <+> ppTyped ppValue tv


ppTypedLabel :: BlockLabel -> Doc
ppTypedLabel i = ppType (PrimType Label) <+> ppLabel i

ppSwitchEntry :: Type -> (Integer,BlockLabel) -> Doc
ppSwitchEntry ty (i,l) = ppType ty <+> integer i <> comma <+> ppTypedLabel l

ppVectorIndex :: Value -> Doc
ppVectorIndex i = ppType (PrimType (Integer 32)) <+> ppValue i

ppAlign :: Maybe Align -> Doc
ppAlign Nothing      = empty
ppAlign (Just align) = comma <+> "align" <+> int align

ppAlloca :: Type -> Maybe (Typed Value) -> Maybe Int -> Doc
ppAlloca ty mbLen mbAlign = "alloca" <+> ppType ty <> len <> align
  where
  len = fromMaybe empty $ do
    l <- mbLen
    return (comma <+> ppTyped ppValue l)
  align = fromMaybe empty $ do
    a <- mbAlign
    return (comma <+> "align" <+> int a)

ppCall :: Bool -> Type -> Value -> [Typed Value] -> Doc
ppCall tc ty f args
  | tc        = "tail" <+> body
  | otherwise = body
  where
  body = "call" <+> ppCallSym ty f
      <> parens (commas (map (ppTyped ppValue) args))

ppCallSym :: Type -> Value -> Doc
ppCallSym (PtrTo (FunTy res _ _)) (ValSymbol sym) = ppType res <+> ppSymbol sym
ppCallSym ty              val                     = ppType ty  <+> ppValue val

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

ppInvoke :: Type -> Value -> [Typed Value] -> BlockLabel -> BlockLabel -> Doc
ppInvoke ty f args to uw = body
  where
  body = "invoke" <+> ppType ty <+> ppValue f
      <> parens (commas (map (ppTyped ppValue) args))
     <+> "to" <+> ppType (PrimType Label) <+> ppLabel to
     <+> "unwind" <+> ppType (PrimType Label) <+> ppLabel uw

ppPhiArg :: (Value,BlockLabel) -> Doc
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

ppValue :: Value -> Doc
ppValue val = case val of
  ValInteger i       -> integer i
  ValBool b          -> ppBool b
  ValFloat i         -> float i
  ValDouble i        -> double i
  ValIdent i         -> ppIdent i
  ValSymbol s        -> ppSymbol s
  ValNull            -> "null"
  ValArray ty es     -> brackets
                      $ commas (map (ppTyped ppValue . Typed ty) es)
  ValVector ty es   -> angles $ commas
                     $ map (ppTyped ppValue . Typed ty) es
  ValStruct fs       -> structBraces (commas (map (ppTyped ppValue) fs))
  ValPackedStruct fs -> angles
                      $ structBraces (commas (map (ppTyped ppValue) fs))
  ValString s        -> char 'c' <> ppStringLiteral s
  ValConstExpr ce    -> ppConstExpr ce
  ValUndef           -> "undef"
  ValLabel l         -> ppLabel l
  ValZeroInit        -> "zeroinitializer"
  ValAsm s a i c     -> ppAsm s a i c
  ValMd m            -> ppValMd m

ppValMd :: ValMd -> Doc
ppValMd m = case m of
  ValMdString str   -> ppMetadata (ppStringLiteral str)
  ValMdValue tv     -> ppTyped ppValue tv
  ValMdRef i        -> ppMetadata (int i)
  ValMdNode vs      -> ppMetadataNode vs
  ValMdLoc l        -> ppDebugLoc l
  ValMdDebugInfo di -> ppDebugInfo di

ppDebugLoc :: DebugLoc -> Doc
ppDebugLoc dl = "!MDLocation"
             <> parens (commas [ "line:"   <+> integral (dlLine dl)
                               , "column:" <+> integral (dlCol dl)
                               , "scope:"  <+> ppValMd (dlScope dl)
                               ] <+> mbIA)

  where
  mbIA = case dlIA dl of
           Just md -> comma <+> "inlinedAt:" <+> ppValMd md
           Nothing -> empty

ppTypedValMd :: ValMd -> Doc
ppTypedValMd  = ppTyped ppValMd . Typed (PrimType Metadata)

ppMetadata :: Doc -> Doc
ppMetadata body = char '!' <> body

ppMetadataNode :: [Maybe ValMd] -> Doc
ppMetadataNode vs = ppMetadata (braces (commas (map arg vs)))
  where
  arg = maybe ("null") ppValMd

ppStringLiteral :: String -> Doc
ppStringLiteral  = doubleQuotes . text . concatMap escape
  where
  escape c | isAscii c && isPrint c = [c]
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


ppConstExpr :: ConstExpr -> Doc
ppConstExpr (ConstGEP inb mp ixs)  = "getelementptr"
  <+> opt inb "inbounds"
  <+> parens (mcommas ((ppType <$> mp) : (map (pure . ppTyped ppValue) ixs)))
ppConstExpr (ConstConv op tv t) = ppConvOp op <+> parens
                                 (ppTyped ppValue tv <+> "to" <+> ppType t)
ppConstExpr (ConstSelect c l r) = "select" <+> parens
                                 (commas [ ppTyped ppValue c, ppTyped ppValue l
                                         , ppTyped ppValue r])
ppConstExpr (ConstBlockAddr t l)= "blockaddress" <+> parens
                                 (ppSymbol t <> comma <+> ppLabel l)

ppConstExpr (ConstFCmp op a b)  = "fcmp" <+> ppFCmpOp op <+> parens
                                 (ppTyped ppValue a <> comma <+> ppTyped ppValue b)

ppConstExpr (ConstICmp op a b)  = "icmp" <+> ppICmpOp op <+> parens
                                 (ppTyped ppValue a <> comma <+> ppTyped ppValue b)
ppConstExpr (ConstArith op a b) = ppArithOp op <+> parens
                                  (ppTyped ppValue a <> comma <+> ppValue b)
ppConstExpr (ConstBit op a b)   = ppBitOp op <+> parens
                                  (ppTyped ppValue a <> comma <+> ppValue b)

-- DWARF Debug Info ------------------------------------------------------------

ppDebugInfo :: DebugInfo -> Doc
ppDebugInfo di = case di of
  DebugInfoBasicType bt         -> ppDIBasicType bt
  DebugInfoCompileUnit cu       -> ppDICompileUnit cu
  DebugInfoCompositeType ct     -> ppDICompositeType ct
  DebugInfoDerivedType dt       -> ppDIDerivedType dt
  DebugInfoEnumerator nm v      -> ppDIEnumerator nm v
  DebugInfoExpression e         -> ppDIExpression e
  DebugInfoFile f               -> ppDIFile f
  DebugInfoGlobalVariable gv    -> ppDIGlobalVariable gv
  DebugInfoGlobalVariableExpression gv -> ppDIGlobalVariableExpression gv
  DebugInfoLexicalBlock lb      -> ppDILexicalBlock lb
  DebugInfoLexicalBlockFile lbf -> ppDILexicalBlockFile lbf
  DebugInfoLocalVariable lv     -> ppDILocalVariable lv
  DebugInfoSubprogram sp        -> ppDISubprogram sp
  DebugInfoSubrange sr          -> ppDISubrange sr
  DebugInfoSubroutineType st    -> ppDISubroutineType st

ppDIBasicType :: DIBasicType -> Doc
ppDIBasicType bt = "!DIBasicType"
  <> parens (commas [ "tag:"      <+> integral (dibtTag bt)
                    , "name:"     <+> doubleQuotes (text (dibtName bt))
                    , "size:"     <+> integral (dibtSize bt)
                    , "align:"    <+> integral (dibtAlign bt)
                    , "encoding:" <+> integral (dibtEncoding bt)
                    ])

ppDICompileUnit :: DICompileUnit -> Doc
ppDICompileUnit cu = "!DICompileUnit"
  <> parens (mcommas
       [ pure ("language:"           <+> integral (dicuLanguage cu))
       ,     (("file:"               <+>) . ppValMd) <$> (dicuFile cu)
       ,     (("producer:"           <+>) . doubleQuotes . text)
             <$> (dicuProducer cu)
       , pure ("isOptimized:"        <+> ppBool (dicuIsOptimized cu))
       , pure ("flags:"              <+> integral (dicuFlags cu))
       , pure ("runtimeVersion:"     <+> integral (dicuRuntimeVersion cu))
       ,     (("splitDebugFilename:" <+>) . doubleQuotes . text)
             <$> (dicuSplitDebugFilename cu)
       , pure ("emissionKind:"       <+> integral (dicuEmissionKind cu))
       ,     (("enums:"              <+>) . ppValMd) <$> (dicuEnums cu)
       ,     (("retainedTypes:"      <+>) . ppValMd) <$> (dicuRetainedTypes cu)
       ,     (("subprograms:"        <+>) . ppValMd) <$> (dicuSubprograms cu)
       ,     (("globals:"            <+>) . ppValMd) <$> (dicuGlobals cu)
       ,     (("imports:"            <+>) . ppValMd) <$> (dicuImports cu)
       ,     (("macros:"             <+>) . ppValMd) <$> (dicuMacros cu)
       , pure ("dwoId:"              <+> integral (dicuDWOId cu))
       ])

ppDICompositeType :: DICompositeType -> Doc
ppDICompositeType ct = "!DICompositeType"
  <> parens (mcommas
       [ pure ("tag:"            <+> integral (dictTag ct))
       ,     (("name:"           <+>) . doubleQuotes . text) <$> (dictName ct)
       ,     (("file:"           <+>) . ppValMd) <$> (dictFile ct)
       , pure ("line:"           <+> integral (dictLine ct))
       ,     (("baseType:"       <+>) . ppValMd) <$> (dictBaseType ct)
       , pure ("size:"           <+> integral (dictSize ct))
       , pure ("align:"          <+> integral (dictAlign ct))
       , pure ("offset:"         <+> integral (dictOffset ct))
       , pure ("flags:"          <+> integral (dictFlags ct))
       ,     (("elements:"       <+>) . ppValMd) <$> (dictElements ct)
       , pure ("runtimeLang:"    <+> integral (dictRuntimeLang ct))
       ,     (("vtableHolder:"   <+>) . ppValMd) <$> (dictVTableHolder ct)
       ,     (("templateParams:" <+>) . ppValMd) <$> (dictTemplateParams ct)
       ,     (("identifier:"     <+>) . doubleQuotes . text)
             <$> (dictIdentifier ct)
       ])

ppDIDerivedType :: DIDerivedType -> Doc
ppDIDerivedType dt = "!DIDerivedType"
  <> parens (mcommas
       [ pure ("tag:"       <+> integral (didtTag dt))
       ,     (("name:"      <+>) . doubleQuotes . text) <$> (didtName dt)
       ,     (("file:"      <+>) . ppValMd) <$> (didtFile dt)
       , pure ("line:"      <+> integral (didtLine dt))
       ,     (("baseType:"  <+>) . ppValMd) <$> (didtBaseType dt)
       , pure ("size:"      <+> integral (didtSize dt))
       , pure ("align:"     <+> integral (didtAlign dt))
       , pure ("offset:"    <+> integral (didtOffset dt))
       , pure ("flags:"     <+> integral (didtFlags dt))
       ,     (("extraData:" <+>) . ppValMd) <$> (didtExtraData dt)
       ])

ppDIEnumerator :: String -> Int64 -> Doc
ppDIEnumerator n v = "!DIEnumerator"
  <> parens (commas [ "name:"  <+> doubleQuotes (text n)
                    , "value:" <+> integral v
                    ])

ppDIExpression :: DIExpression -> Doc
ppDIExpression e = "!DIExpression"
  <> parens (commas (map integral (dieElements e)))

ppDIFile :: DIFile -> Doc
ppDIFile f = "!DIFile"
  <> parens (commas [ "filename:"  <+> doubleQuotes (text (difFilename f))
                    , "directory:" <+> doubleQuotes (text (difDirectory f))
                    ])

ppDIGlobalVariable :: DIGlobalVariable -> Doc
ppDIGlobalVariable gv = "!DIGlobalVariable"
  <> parens (mcommas
       [      (("scope:"       <+>) . ppValMd) <$> (digvScope gv)
       ,      (("name:"        <+>) . doubleQuotes . text) <$> (digvName gv)
       ,      (("linkageName:" <+>) . doubleQuotes . text)
              <$> (digvLinkageName gv)
       ,      (("file:"        <+>) . ppValMd) <$> (digvFile gv)
       , pure ("line:"         <+> integral (digvLine gv))
       ,      (("type:"        <+>) . ppValMd) <$> (digvType gv)
       , pure ("isLocal:"      <+> ppBool (digvIsLocal gv))
       , pure ("isDefinition:" <+> ppBool (digvIsDefinition gv))
       ,      (("variable:"    <+>) . ppValMd) <$> (digvType gv)
       ,      (("declaration:" <+>) . ppValMd) <$> (digvDeclaration gv)
       ,      (("align:"       <+>) . integral) <$> digvAlignment gv
       ])

ppDIGlobalVariableExpression :: DIGlobalVariableExpression -> Doc
ppDIGlobalVariableExpression gve = "!DIGlobalVariableExpression"
  <> parens (mcommas
       [      (("var:"  <+>) . ppValMd) <$> (digveVariable gve)
       ,      (("expr:" <+>) . ppValMd) <$> (digveExpression gve)
       ])

ppDILexicalBlock :: DILexicalBlock -> Doc
ppDILexicalBlock ct = "!DILexicalBlock"
  <> parens (mcommas
       [     (("scope:"  <+>) . ppValMd) <$> (dilbScope ct)
       ,     (("file:"   <+>) . ppValMd) <$> (dilbFile ct)
       , pure ("line:"   <+> integral (dilbLine ct))
       , pure ("column:" <+> integral (dilbColumn ct))
       ])

ppDILexicalBlockFile :: DILexicalBlockFile -> Doc
ppDILexicalBlockFile lbf = "!DILexicalBlockFile"
  <> parens (mcommas
       [ pure ("scope:"         <+> ppValMd (dilbfScope lbf))
       ,     (("file:"          <+>) . ppValMd) <$> (dilbfFile lbf)
       , pure ("discriminator:" <+> integral (dilbfDiscriminator lbf))
       ])

ppDILocalVariable :: DILocalVariable -> Doc
ppDILocalVariable lv = "!DILocalVariable"
  <> parens (mcommas
       [      (("scope:" <+>) . ppValMd) <$> (dilvScope lv)
       ,      (("name:"  <+>) . doubleQuotes . text) <$> (dilvName lv)
       ,      (("file:"  <+>) . ppValMd) <$> (dilvFile lv)
       , pure ("line:"   <+> integral (dilvLine lv))
       ,      (("type:"  <+>) . ppValMd) <$> (dilvType lv)
       , pure ("arg:"    <+> integral (dilvArg lv))
       , pure ("flags:"  <+> integral (dilvFlags lv))
       ])

ppDISubprogram :: DISubprogram -> Doc
ppDISubprogram sp = "!DISubprogram"
  <> parens (mcommas
       [      (("scope:"          <+>) . ppValMd) <$> (dispScope sp)
       ,      (("name:"           <+>) . doubleQuotes . text) <$> (dispName sp)
       ,      (("linkageName:"    <+>) . doubleQuotes . text)
              <$> (dispLinkageName sp)
       ,      (("file:"           <+>) . ppValMd) <$> (dispFile sp)
       , pure ("line:"            <+> integral (dispLine sp))
       ,      (("type:"           <+>) . ppValMd) <$> (dispType sp)
       , pure ("isLocal:"         <+> ppBool (dispIsLocal sp))
       , pure ("isDefinition:"    <+> ppBool (dispIsDefinition sp))
       , pure ("scopeLine:"       <+> integral (dispScopeLine sp))
       ,      (("containingType:" <+>) . ppValMd) <$> (dispContainingType sp)
       , pure ("virtuality:"      <+> integral (dispVirtuality sp))
       , pure ("virtualIndex:"    <+> integral (dispVirtualIndex sp))
       , pure ("flags:"           <+> integral (dispFlags sp))
       , pure ("isOptimized:"     <+> ppBool (dispIsOptimized sp))
       ,      (("templateParams:" <+>) . ppValMd) <$> (dispTemplateParams sp)
       ,      (("declaration:"    <+>) . ppValMd) <$> (dispDeclaration sp)
       ,      (("variables:"      <+>) . ppValMd) <$> (dispVariables sp)
       ])

ppDISubrange :: DISubrange -> Doc
ppDISubrange sr = "!DISubrange"
  <> parens (commas [ "count:" <+> integral (disrCount sr)
                    , "lowerBound:" <+> integral (disrLowerBound sr)
                    ])

ppDISubroutineType :: DISubroutineType -> Doc
ppDISubroutineType st = "!DISubroutineType"
  <> parens (commas
       [ "flags:" <+> integral (distFlags st)
       , "types:" <+> fromMaybe "null" (ppValMd <$> (distTypeArray st))
       ])

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
