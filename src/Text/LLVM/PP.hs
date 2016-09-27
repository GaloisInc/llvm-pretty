-- |
-- Module      :  Text.LLVM.PP36
-- Copyright   :  Trevor Elliott 2011-2016
-- License     :  BSD3
--
-- Maintainer  :  awesomelyawesome@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- This is the pretty-printer for llvm assembly versions 3.6 and lower.
--
module Text.LLVM.PP (
    module Text.LLVM.PP
  , ppLLVM
  , ppLLVM35
  , ppLLVM36
  , ppLLVM37
  ) where

import Text.LLVM.AST
import Text.LLVM.PP.Core

import Data.Char (isAscii,isPrint,ord,toUpper)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Numeric (showHex)


-- Modules ---------------------------------------------------------------------

ppModule :: Module -> Doc
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
      , distinct <+> ppMetadataNode (umValues um) ]
  where
  distinct | umDistinct um = text "distinct"
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
ppDataLayout ls = text "target" <+> text "datalayout" <+> char '='
    <+> doubleQuotes (hcat (intersperse (char '-') (map ppLayoutSpec ls)))

-- | Pretty print a single layout specification.
ppLayoutSpec :: LayoutSpec -> Doc
ppLayoutSpec  BigEndian                  = char 'E'
ppLayoutSpec  LittleEndian               = char 'e'
ppLayoutSpec (PointerSize   sz abi pref) = text "p:" <> ppLayoutBody sz abi pref
ppLayoutSpec (IntegerSize   sz abi pref) = char 'i'  <> ppLayoutBody sz abi pref
ppLayoutSpec (VectorSize    sz abi pref) = char 'v'  <> ppLayoutBody sz abi pref
ppLayoutSpec (FloatSize     sz abi pref) = char 'f'  <> ppLayoutBody sz abi pref
ppLayoutSpec (AggregateSize sz abi pref) = char 'a'  <> ppLayoutBody sz abi pref
ppLayoutSpec (StackObjSize  sz abi pref) = char 's'  <> ppLayoutBody sz abi pref
ppLayoutSpec (NativeIntSize szs)         = char 'n'  <> colons (map int szs)
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
  ppLine l = text "module asm" <+> doubleQuotes (text l)


-- Identifiers -----------------------------------------------------------------

ppIdent :: Ident -> Doc
ppIdent (Ident n) = char '%' <> text n


-- Symbols ---------------------------------------------------------------------

ppSymbol :: Symbol -> Doc
ppSymbol (Symbol n) = char '@' <> text n


-- Types -----------------------------------------------------------------------

ppPrimType :: PrimType -> Doc
ppPrimType Label          = text "label"
ppPrimType Void           = text "void"
ppPrimType (Integer i)    = char 'i' <> integer (toInteger i)
ppPrimType (FloatType ft) = ppFloatType ft
ppPrimType X86mmx         = text "x86mmx"
ppPrimType Metadata       = text "metadata"

ppFloatType :: FloatType -> Doc
ppFloatType Half      = text "half"
ppFloatType Float     = text "float"
ppFloatType Double    = text "double"
ppFloatType Fp128     = text "fp128"
ppFloatType X86_fp80  = text "x86_fp80"
ppFloatType PPC_fp128 = text "ppc_fp128"

ppType :: Type -> Doc
ppType (PrimType pt)     = ppPrimType pt
ppType (Alias i)         = ppIdent i
ppType (Array len ty)    = brackets (int32 len <+> char 'x' <+> ppType ty)
ppType (PtrTo ty)        = ppType ty <> char '*'
ppType (Struct ts)       = structBraces (commas (map ppType ts))
ppType (PackedStruct ts) = angles (structBraces (commas (map ppType ts)))
ppType (FunTy r as va)   = ppType r <> ppArgList va (map ppType as)
ppType (Vector len pt)   = angles (int32 len <+> char 'x' <+> ppType pt)
ppType Opaque            = text "opaque"

ppTypeDecl :: TypeDecl -> Doc
ppTypeDecl td = ppIdent (typeName td) <+> char '='
            <+> text "type" <+> ppType (typeValue td)


-- Declarations ----------------------------------------------------------------

ppGlobal :: Global -> Doc
ppGlobal g = ppSymbol (globalSym g) <+> char '='
         <+> ppGlobalAttrs (globalAttrs g)
         <+> ppType (globalType g) <+> ppMaybe ppValue (globalValue g)
          <> ppAlign (globalAlign g)

ppGlobalAttrs :: GlobalAttrs -> Doc
ppGlobalAttrs ga = ppMaybe ppLinkage (gaLinkage ga) <+> constant
  where
  constant | gaConstant ga = text "constant"
           | otherwise     = text "global"


ppDeclare :: Declare -> Doc
ppDeclare d = text "declare"
          <+> ppType (decRetType d)
          <+> ppSymbol (decName d)
           <> ppArgList (decVarArgs d) (map ppType (decArgs d))


ppDefine :: Define -> Doc
ppDefine d = text "define"
         <+> ppMaybe ppLinkage (funLinkage (defAttrs d))
         <+> ppType (defRetType d)
         <+> ppSymbol (defName d)
          <> ppArgList (defVarArgs d) (map (ppTyped ppIdent) (defArgs d))
         <+> ppMaybe (\s  -> text "section" <+> doubleQuotes (text s)) (defSection d)
         <+> ppMaybe (\gc -> text "gc" <+> ppGC gc) (funGC (defAttrs d))
         <+> char '{'
         $+$ vcat (map ppBasicBlock (defBody d))
         $+$ char '}'


-- Basic Blocks ----------------------------------------------------------------

ppLabelDef :: BlockLabel -> Doc
ppLabelDef (Named (Ident l)) = text l <> char ':'
ppLabelDef (Anon i)          = char ';' <+> text "<label>:" <+> int i

ppLabel :: BlockLabel -> Doc
ppLabel (Named l) = ppIdent l
ppLabel (Anon i)  = char '%' <> int i

ppBasicBlock :: BasicBlock -> Doc
ppBasicBlock bb = ppMaybe ppLabelDef (bbLabel bb)
              $+$ nest 2 (vcat (map ppStmt (bbStmts bb)))


-- Statements ------------------------------------------------------------------

ppStmt :: Stmt -> Doc
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
  Private                  -> text "private"
  LinkerPrivate            -> text "linker_private"
  LinkerPrivateWeak        -> text "linker_private_weak"
  LinkerPrivateWeakDefAuto -> text "linker_private_weak_def_auto"
  Internal                 -> text "internal"
  AvailableExternally      -> text "available_externally"
  Linkonce                 -> text "linkonce"
  Weak                     -> text "weak"
  Common                   -> text "common"
  Appending                -> text "appending"
  ExternWeak               -> text "extern_weak"
  LinkonceODR              -> text "linkonce_ddr"
  WeakODR                  -> text "weak_odr"
  External                 -> text "external"
  DLLImport                -> text "dllimport"
  DLLExport                -> text "dllexport"

ppGC :: GC -> Doc
ppGC  = doubleQuotes . text . getGC


-- Expressions -----------------------------------------------------------------

ppTyped :: (a -> Doc) -> Typed a -> Doc
ppTyped fmt ty = ppType (typedType ty) <+> fmt (typedValue ty)

ppSignBits :: Bool -> Bool -> Doc
ppSignBits nuw nsw = opt nuw (text "nuw") <+> opt nsw (text "nsw")

ppExact :: Bool -> Doc
ppExact e = opt e (text "exact")

ppArithOp :: ArithOp -> Doc
ppArithOp (Add nuw nsw) = text "add" <+> ppSignBits nuw nsw
ppArithOp FAdd          = text "fadd"
ppArithOp (Sub nuw nsw) = text "sub" <+> ppSignBits nuw nsw
ppArithOp FSub          = text "fsub"
ppArithOp (Mul nuw nsw) = text "mul" <+> ppSignBits nuw nsw
ppArithOp FMul          = text "fmul"
ppArithOp (UDiv e)      = text "udiv" <+> ppExact e
ppArithOp (SDiv e)      = text "sdiv" <+> ppExact e
ppArithOp FDiv          = text "fdiv"
ppArithOp URem          = text "urem"
ppArithOp SRem          = text "srem"
ppArithOp FRem          = text "frem"

ppBitOp :: BitOp -> Doc
ppBitOp (Shl nuw nsw) = text "shl"  <+> ppSignBits nuw nsw
ppBitOp (Lshr e)      = text "lshr" <+> ppExact e
ppBitOp (Ashr e)      = text "ashr" <+> ppExact e
ppBitOp And           = text "and"
ppBitOp Or            = text "or"
ppBitOp Xor           = text "xor"

ppConvOp :: ConvOp -> Doc
ppConvOp Trunc    = text "trunc"
ppConvOp ZExt     = text "zext"
ppConvOp SExt     = text "sext"
ppConvOp FpTrunc  = text "fptrunc"
ppConvOp FpExt    = text "fpext"
ppConvOp FpToUi   = text "fptoui"
ppConvOp FpToSi   = text "fptosi"
ppConvOp UiToFp   = text "uitofp"
ppConvOp SiToFp   = text "sitofp"
ppConvOp PtrToInt = text "ptrtoint"
ppConvOp IntToPtr = text "inttoptr"
ppConvOp BitCast  = text "bitcast"

ppInstr :: Instr -> Doc
ppInstr instr = case instr of
  Ret tv                 -> text "ret" <+> ppTyped ppValue tv
  RetVoid                -> text "ret void"
  Arith op l r           -> ppArithOp op <+> ppTyped ppValue l
                         <> comma <+> ppValue r
  Bit op l r             -> ppBitOp op <+> ppTyped ppValue l
                         <> comma <+> ppValue r
  Conv op a ty           -> ppConvOp op <+> ppTyped ppValue a
                        <+> text "to" <+> ppType ty
  Call tc ty f args      -> ppCall tc ty f args
  Alloca ty len align    -> ppAlloca ty len align
  Load ptr ma            -> ppLoad ptr ma
  Store a ptr ma         -> text "store" <+> ppTyped ppValue a
                         <> comma <+> ppTyped ppValue ptr
                         <> ppAlign ma
  ICmp op l r            -> text "icmp" <+> ppICmpOp op
                        <+> ppTyped ppValue l <> comma <+> ppValue r
  FCmp op l r            -> text "fcmp" <+> ppFCmpOp op
                        <+> ppTyped ppValue l <> comma <+> ppValue r
  Phi ty vls             -> text "phi" <+> ppType ty
                        <+> commas (map ppPhiArg vls)
  Select c t f           -> text "select" <+> ppTyped ppValue c
                         <> comma <+> ppTyped ppValue t
                         <> comma <+> ppTyped ppValue (f <$ t)
  ExtractValue v is      -> text "extractvalue" <+> ppTyped ppValue v
                         <> comma <+> (commas (map int32 is))
  InsertValue a v is     -> text "insertvalue" <+> ppTyped ppValue a
                         <> comma <+> ppTyped ppValue v
                         <> comma <+> commas (map int32 is)
  ShuffleVector a b m    -> text "shufflevector" <+> ppTyped ppValue a
                         <> comma <+> ppTyped ppValue (b <$ a)
                         <> comma <+> ppTyped ppValue m
  GEP ib ptr ixs         -> ppGEP ib ptr ixs
  Comment str            -> char ';' <+> text str
  Jump i                 -> text "br"
                        <+> ppTypedLabel i
  Br c t f               -> text "br" <+> ppTyped ppValue c
                         <> comma <+> ppType (PrimType Label)
                        <+> ppLabel t
                         <> comma <+> ppType (PrimType Label)
                        <+> ppLabel f
  Invoke ty f args to uw -> ppInvoke ty f args to uw
  Unreachable            -> text "unreachable"
  Unwind                 -> text "unwind"
  VaArg al t             -> text "va_arg" <+> ppTyped ppValue al
                         <> comma <+> ppType t
  ExtractElt v i         -> text "extractelement"
                        <+> ppTyped ppValue v
                         <> comma <+> ppVectorIndex i
  InsertElt v e i        -> text "insertelement"
                        <+> ppTyped ppValue v
                         <> comma <+> ppTyped ppValue e
                         <> comma <+> ppVectorIndex i
  IndirectBr d ls        -> text "indirectbr"
                        <+> ppTyped ppValue d
                         <> comma <+> commas (map ppTypedLabel ls)
  Switch c d ls          -> text "switch"
                        <+> ppTyped ppValue c
                         <> comma <+> ppTypedLabel d
                        <+> char '['
                         $$ nest 2 (vcat (map (ppSwitchEntry (typedType c)) ls))
                         $$ char ']'
  LandingPad ty fn c cs  -> text "landingpad"
                        <+> ppType ty
                        <+> text "personality"
                        <+> ppTyped ppValue fn
                         $$ nest 2 (ppClauses c cs)
  Resume tv              -> text "resume" <+> ppTyped ppValue tv

ppLoad :: Typed (Value' BlockLabel) -> Maybe Align -> Doc
ppLoad ptr ma =
  do isImplicit <- checkConfig cfgLoadImplicitType

     text "load" <+> (if isImplicit then empty else explicit)
                 <+> ppTyped ppValue ptr
                  <> ppAlign ma

  where
  explicit =
    case typedType ptr of
      PtrTo ty -> ppType ty <> comma
      ty       -> ppType ty <> comma

ppClauses :: Bool -> [Clause] -> Doc
ppClauses isCleanup cs = vcat (cleanup : map ppClause cs)
  where
  cleanup | isCleanup = text "cleanup"
          | otherwise = empty

ppClause :: Clause -> Doc
ppClause c = case c of
  Catch  tv -> text "catch"  <+> ppTyped ppValue tv
  Filter tv -> text "filter" <+> ppTyped ppValue tv


ppTypedLabel :: BlockLabel -> Doc
ppTypedLabel i = ppType (PrimType Label) <+> ppLabel i

ppSwitchEntry :: Type -> (Integer,BlockLabel) -> Doc
ppSwitchEntry ty (i,l) = ppType ty <+> integer i <> comma <+> ppTypedLabel l

ppVectorIndex :: Value -> Doc
ppVectorIndex i = ppType (PrimType (Integer 32)) <+> ppValue i

ppAlign :: Maybe Align -> Doc
ppAlign Nothing      = empty
ppAlign (Just align) = comma <+> text "align" <+> int align

ppAlloca :: Type -> Maybe (Typed Value) -> Maybe Int -> Doc
ppAlloca ty mbLen mbAlign = text "alloca" <+> ppType ty <> len <> align
  where
  len = fromMaybe empty $ do
    l <- mbLen
    return (comma <+> ppTyped ppValue l)
  align = fromMaybe empty $ do
    a <- mbAlign
    return (comma <+> text "align" <+> int a)

ppCall :: Bool -> Type -> Value -> [Typed Value] -> Doc
ppCall tc ty f args
  | tc        = text "tail" <+> body
  | otherwise = body
  where
  body = text "call" <+> ppCallSym ty f
      <> parens (commas (map (ppTyped ppValue) args))

ppCallSym :: Type -> Value -> Doc
ppCallSym (PtrTo (FunTy res _ _)) (ValSymbol sym) = ppType res <+> ppSymbol sym
ppCallSym ty              val                     = ppType ty  <+> ppValue val

ppGEP :: Bool -> Typed Value -> [Typed Value] -> Doc
ppGEP ib ptr ixs = text "getelementptr" <+> inbounds
               <+> commas (map (ppTyped ppValue) (ptr:ixs))
  where
  inbounds | ib        = text "inbounds"
           | otherwise = empty

ppInvoke :: Type -> Value -> [Typed Value] -> BlockLabel -> BlockLabel -> Doc
ppInvoke ty f args to uw = body
  where
  body = text "invoke" <+> ppType ty <+> ppValue f
      <> parens (commas (map (ppTyped ppValue) args))
     <+> text "to" <+> ppType (PrimType Label) <+> ppLabel to
     <+> text "unwind" <+> ppType (PrimType Label) <+> ppLabel uw

ppPhiArg :: (Value,BlockLabel) -> Doc
ppPhiArg (v,l) = char '[' <+> ppValue v <> comma <+> ppLabel l <+> char ']'

ppICmpOp :: ICmpOp -> Doc
ppICmpOp Ieq  = text "eq"
ppICmpOp Ine  = text "ne"
ppICmpOp Iugt = text "ugt"
ppICmpOp Iuge = text "uge"
ppICmpOp Iult = text "ult"
ppICmpOp Iule = text "ule"
ppICmpOp Isgt = text "sgt"
ppICmpOp Isge = text "sge"
ppICmpOp Islt = text "slt"
ppICmpOp Isle = text "sle"

ppFCmpOp :: FCmpOp -> Doc
ppFCmpOp Ffalse = text "false"
ppFCmpOp Foeq   = text "oeq"
ppFCmpOp Fogt   = text "ogt"
ppFCmpOp Foge   = text "oge"
ppFCmpOp Folt   = text "olt"
ppFCmpOp Fole   = text "ole"
ppFCmpOp Fone   = text "one"
ppFCmpOp Ford   = text "ord"
ppFCmpOp Fueq   = text "ueq"
ppFCmpOp Fugt   = text "ugt"
ppFCmpOp Fuge   = text "uge"
ppFCmpOp Fult   = text "ult"
ppFCmpOp Fule   = text "ule"
ppFCmpOp Fune   = text "une"
ppFCmpOp Funo   = text "uno"
ppFCmpOp Ftrue  = text "true"

ppValue :: Value -> Doc
ppValue val = case val of
  ValInteger i       -> integer i
  ValBool b          -> ppBool b
  ValFloat i         -> float i
  ValDouble i        -> double i
  ValIdent i         -> ppIdent i
  ValSymbol s        -> ppSymbol s
  ValNull            -> text "null"
  ValArray ty es     -> brackets
                      $ commas (map (ppTyped ppValue . Typed ty) es)
  ValVector ty es   -> angles $ commas
                     $ map (ppTyped ppValue . Typed ty) es
  ValStruct fs       -> structBraces (commas (map (ppTyped ppValue) fs))
  ValPackedStruct fs -> angles
                      $ structBraces (commas (map (ppTyped ppValue) fs))
  ValString s        -> char 'c' <> ppStringLiteral s
  ValConstExpr ce    -> ppConstExpr ce
  ValUndef           -> text "undef"
  ValLabel l         -> ppLabel l
  ValZeroInit        -> text "zeroinitializer"
  ValAsm s a i c     -> ppAsm s a i c
  ValMd m            -> ppValMd m

ppValMd :: ValMd -> Doc
ppValMd m = case m of
  ValMdString str -> ppMetadata (ppStringLiteral str)
  ValMdValue tv   -> ppTyped ppValue tv
  ValMdRef i      -> ppMetadata (int i)
  ValMdNode vs    -> ppMetadataNode vs
  ValMdLoc l      -> ppDebugLoc l

ppDebugLoc :: DebugLoc -> Doc
ppDebugLoc dl = text "!MDLocation"
             <> parens (commas [ text "line:"    <+> int32 (dlLine dl)
                               , text "column:"  <+> int32 (dlCol dl)
                               , text "scope:"   <+> ppValMd (dlScope dl)
                               ] <+> mbIA)

  where
  mbIA = case dlIA dl of
           Just md -> comma <+> text "inlinedAt:" <+> ppValMd md
           Nothing -> empty

ppTypedValMd :: ValMd -> Doc
ppTypedValMd  = ppTyped ppValMd . Typed (PrimType Metadata)

ppMetadata :: Doc -> Doc
ppMetadata body = char '!' <> body

ppMetadataNode :: [Maybe ValMd] -> Doc
ppMetadataNode vs = ppMetadata (braces (commas (map arg vs)))
  where
  arg = maybe (text "null") ppValMd

ppBool :: Bool -> Doc
ppBool b | b         = text "true"
         | otherwise = text "false"

ppStringLiteral :: String -> Doc
ppStringLiteral  = doubleQuotes . text . concatMap escape
  where
  escape c | isAscii c && isPrint c = [c]
           | otherwise              = '\\' : pad (ord c)

  pad n | n < 0x10  = '0' : map toUpper (showHex n "")
        | otherwise =       map toUpper (showHex n "")

ppAsm :: Bool -> Bool -> String -> String -> Doc
ppAsm s a i c =
  text "asm" <+> sideeffect <+> alignstack
             <+> ppStringLiteral i <> comma <+> ppStringLiteral c
  where
  sideeffect | s         = text "sideeffect"
             | otherwise = empty

  alignstack | a         = text "alignstack"
             | otherwise = empty


ppConstExpr :: ConstExpr -> Doc
ppConstExpr (ConstGEP inb ixs)  = text "getelementptr"
                              <+> opt inb (text "inbounds")
                              <+> parens (commas (map (ppTyped ppValue) ixs))
ppConstExpr (ConstConv op tv t) = ppConvOp op <+> parens
                                 (ppTyped ppValue tv <+> text "to" <+> ppType t)
ppConstExpr (ConstSelect c l r) = text "select" <+> parens
                                 (commas [ ppTyped ppValue c, ppTyped ppValue l
                                         , ppTyped ppValue r])
ppConstExpr (ConstBlockAddr t l)= text "blockaddress" <+> parens
                                 (ppSymbol t <> comma <+> ppLabel l)


-- Utilities -------------------------------------------------------------------

-- | Build a variable-argument argument list.
ppArgList :: Bool -> [Doc] -> Doc
ppArgList True  ds = parens (commas (ds ++ [text "..."]))
ppArgList False ds = parens (commas ds)
