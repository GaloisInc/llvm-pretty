module Text.LLVM.AST where

import Data.Int (Int32)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Text.PrettyPrint.HughesPJ

commas :: [Doc] -> Doc
commas ds = hcat (intersperse (comma <> space) ds)

int32 :: Int32 -> Doc
int32  = integer . fromIntegral

angles :: Doc -> Doc
angles d = char '<' <> d <> char '>'

ppMaybe :: (a -> Doc) -> Maybe a -> Doc
ppMaybe  = maybe empty

-- Modules ---------------------------------------------------------------------

data Module = Module
  { modTypes    :: [TypeDecl]
  , modGlobals  :: [Global]
  , modDeclares :: [Declare]
  , modDefines  :: [Define]
  } deriving (Show)

instance Monoid Module where
  mempty = emptyModule
  mappend m1 m2 = Module
    { modTypes    = modTypes    m1 ++ modTypes    m2
    , modGlobals  = modGlobals  m1 ++ modGlobals  m2
    , modDeclares = modDeclares m1 ++ modDeclares m2
    , modDefines  = modDefines  m1 ++ modDefines  m2
    }

emptyModule :: Module
emptyModule  = Module
  { modTypes    = []
  , modGlobals  = []
  , modDeclares = []
  , modDefines  = []
  }

ppModule :: Module -> Doc
ppModule m = vcat $ concat
  [ map ppTypeDecl (modTypes m)
  , map ppDeclare (modDeclares m)
  , map ppDefine (modDefines m)
  ]

-- Identifiers -----------------------------------------------------------------

newtype Ident = Ident String
    deriving (Show,Eq,Ord)

ppIdent :: Ident -> Doc
ppIdent (Ident n) = char '%' <> text n

-- Symbols ---------------------------------------------------------------------

newtype Symbol = Symbol String
    deriving (Show,Eq,Ord)

ppSymbol :: Symbol -> Doc
ppSymbol (Symbol n) = char '@' <> text n

-- Types -----------------------------------------------------------------------

data PrimType
  = Label
  | Void
  | Integer Int32
  | FloatType FloatType
  | X86mmx
  | Metadata
    deriving (Show,Eq)

ppPrimType :: PrimType -> Doc
ppPrimType Label          = text "label"
ppPrimType Void           = text "void"
ppPrimType (Integer i)    = char 'i' <> integer (toInteger i)
ppPrimType (FloatType ft) = ppFloatType ft
ppPrimType X86mmx         = text "x86mmx"
ppPrimType Metadata       = text "metadata"

data FloatType
  = Float
  | Double
  | Fp128
  | X86_fp80
  | PPC_fp128
    deriving (Show,Eq)

ppFloatType :: FloatType -> Doc
ppFloatType Float     = text "float"
ppFloatType Double    = text "double"
ppFloatType Fp128     = text "fp128"
ppFloatType X86_fp80  = text "x86_fp80"
ppFloatType PPC_fp128 = text "ppc_fp128"

data Type
  = PrimType PrimType
  | Alias Ident
  | Array Int32 Type
  | FunTy Type [Type]
  | PtrTo Type
  | Struct [Type]
  | PackedStruct [Type]
  | Vector Int32 PrimType
  | Opaque
    deriving (Show)

ppType :: Type -> Doc
ppType (PrimType pt)     = ppPrimType pt
ppType (Alias i)         = ppIdent i
ppType (Array len ty)    = brackets (int32 len <+> char 'x' <+> ppType ty)
ppType (PtrTo ty)        = ppType ty <> char '*'
ppType (Struct ts)       = braces (commas (map ppType ts))
ppType (PackedStruct ts) = angles (braces (commas (map ppType ts)))
ppType (FunTy r as)      = ppType r <> parens (commas (map ppType as))
ppType (Vector len pt)   = angles (int32 len <+> char 'x' <+> ppPrimType pt)
ppType Opaque            = text "opaque"

iTy :: Int32 -> Type
iTy  = PrimType . Integer

floatTy :: FloatType -> Type
floatTy  = PrimType . FloatType

-- Top-level Type Aliases ------------------------------------------------------

data TypeDecl = TypeDecl
  { typeName  :: Ident
  , typeValue :: Type
  } deriving (Show)

ppTypeDecl :: TypeDecl -> Doc
ppTypeDecl td = ppIdent (typeName td) <+> char '='
            <+> text "type" <+> ppType (typeValue td)

-- Globals ---------------------------------------------------------------------

data Global = Global
  { globalSym   :: Symbol
  , globalType  :: Type
  , globalValue :: Value
  } deriving Show

ppGlobal :: Global -> Doc
ppGlobal g = ppSymbol (globalSym g) <+> char '=' <+> text "global"
         <+> ppType (globalType g) <+> ppValue (globalValue g)

-- Declarations ----------------------------------------------------------------

data Declare = Declare
  { decRetType :: Type
  , decName    :: Symbol
  , decArgs    :: [Type]
  } deriving (Show)

ppDeclare :: Declare -> Doc
ppDeclare d = text "declare"
          <+> ppType (decRetType d)
          <+> ppSymbol (decName d) <> parens (commas (map ppType (decArgs d)))

-- Function Definitions --------------------------------------------------------

data Define = Define
  { defLinkage :: Maybe Linkage
  , defRetType :: Type
  , defName    :: Symbol
  , defArgs    :: [Typed Ident]
  , defGC      :: Maybe GC
  , defBody    :: [BasicBlock]
  } deriving (Show)

ppDefine :: Define -> Doc
ppDefine d = text "define"
         <+> ppMaybe ppLinkage (defLinkage d)
         <+> ppType (defRetType d)
         <+> ppSymbol (defName d)
          <> parens (commas (map (ppTyped ppIdent) (defArgs d)))
         <+> ppMaybe (\gc -> text "gc" <+> ppGC gc) (defGC d)
         <+> char '{'
         $+$ nest 2 (vcat (map ppBasicBlock (defBody d)))
         $+$ char '}'

-- Basic Blocks ----------------------------------------------------------------

data BasicBlock = BasicBlock
  { bbLabel :: Maybe Ident
  , bbStmts :: [Stmt]
  } deriving (Show)

anonBasicBlock :: [Stmt] -> BasicBlock
anonBasicBlock  = BasicBlock Nothing

ppBasicBlock :: BasicBlock -> Doc
ppBasicBlock bb = maybe empty ppLabelDef (bbLabel bb)
              $+$ vcat (map ppStmt (bbStmts bb))

ppLabelDef :: Ident -> Doc
ppLabelDef (Ident l) = text l <> char ':'

-- Attributes ------------------------------------------------------------------

-- | Symbol Linkage
data Linkage
  = Private
  | LinkerPrivate
  | LinkerPrivateWeak
  | LinkerPrivateWeakDefAuto
  | Internal
  | AvailableExternally
  | Linkonce
  | Weak
  | Common
  | Appending
  | ExternWeak
  | LinkonceODR
  | WeakODR
  | DLLImport
  | DLLExport
    deriving (Show)

ppLinkage :: Linkage -> Doc
ppLinkage Private                  = text "private"
ppLinkage LinkerPrivate            = text "linker_private"
ppLinkage LinkerPrivateWeak        = text "linker_private_weak"
ppLinkage LinkerPrivateWeakDefAuto = text "linker_private_weak_def_auto"
ppLinkage Internal                 = text "internal"
ppLinkage AvailableExternally      = text "available_externally"
ppLinkage Linkonce                 = text "linkonce"
ppLinkage Weak                     = text "weak"
ppLinkage Common                   = text "common"
ppLinkage Appending                = text "appending"
ppLinkage ExternWeak               = text "extern_weak"
ppLinkage LinkonceODR              = text "linkonce_ddr"
ppLinkage WeakODR                  = text "weak_odr"
ppLinkage DLLImport                = text "dllimport"
ppLinkage DLLExport                = text "dllexport"

newtype GC = GC
  { getGC :: String
  } deriving (Show)

ppGC :: GC -> Doc
ppGC  = doubleQuotes . text . getGC

-- Typed Things ----------------------------------------------------------------

data Typed a = Typed
  { typedType  :: Type
  , typedValue :: a
  } deriving Show

instance Functor Typed where
  fmap f t = t { typedValue = f (typedValue t) }

ppTyped :: (a -> Doc) -> Typed a -> Doc
ppTyped fmt ty = ppType (typedType ty) <+> fmt (typedValue ty)

-- Instructions ----------------------------------------------------------------

data ArithOp
  = Add  | FAdd
  | Sub  | FSub
  | Mul  | FMul
  | UDiv | SDiv | FDiv
  | URem | SRem | FRem
    deriving (Eq,Show)

ppArithOp :: ArithOp -> Doc
ppArithOp Add  = text "add"
ppArithOp FAdd = text "fadd"
ppArithOp Sub  = text "sub"
ppArithOp FSub = text "fsub"
ppArithOp Mul  = text "mul"
ppArithOp FMul = text "fmul"
ppArithOp UDiv = text "udiv"
ppArithOp SDiv = text "sdiv"
ppArithOp FDiv = text "fdiv"
ppArithOp URem = text "urem"
ppArithOp SRem = text "srem"
ppArithOp FRem = text "frem"

data BitOp
  = Shl | Lshr | Ashr
  | And | Or   | Xor
    deriving Show

ppBitOp :: BitOp -> Doc
ppBitOp Shl  = text "shl"
ppBitOp Lshr = text "lshr"
ppBitOp Ashr = text "ashr"
ppBitOp And  = text "and"
ppBitOp Or   = text "or"
ppBitOp Xor  = text "xor"

data ConvOp
  = Trunc
  | ZExt
  | SExt
  | FpTrunc
  | FpExt
  | FpToUi
  | FpToSi
  | UiToFp
  | SiToFp
  | PtrToInt
  | IntToPtr
  | BitCast
    deriving Show

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

data Instr
  = Ret (Typed Value)
  | RetVoid
  | Arith ArithOp (Typed Value) Value
  | Bit BitOp (Typed Value) Value
  | Conv ConvOp (Typed Value) Type
  | GenInstr String [Arg]
  | Call Bool Type Value [Typed Value]
  | Alloca Type (Maybe (Typed Value)) (Maybe Int)
  | ICmp ICmpOp (Typed Value) Value
  | FCmp FCmpOp (Typed Value) Value
  | Phi Type [(Value,Ident)]
  | Select (Typed Value) (Typed Value) Value
  | Comment String
    deriving (Show)

isTerminator :: Instr -> Bool
isTerminator Ret{}   = True
isTerminator RetVoid = True
isTerminator _       = False

isComment :: Instr -> Bool
isComment Comment{} = True
isComment _         = False

ppInstr :: Instr -> Doc
ppInstr (Ret tv)              = text "ret" <+> ppTyped ppValue tv
ppInstr RetVoid               = text "ret void"
ppInstr (Arith op l r)        = ppArithOp op <+> ppTyped ppValue l
                             <> comma <+> ppValue r
ppInstr (Bit op l r)          = ppBitOp op <+> ppTyped ppValue l
                             <> comma <+> ppValue r
ppInstr (Conv op a ty)        = ppConvOp op <+> ppTyped ppValue a
                            <+> text "to" <+> ppType ty
ppInstr (GenInstr op args)    = text op <+> commas (map ppArg args)
ppInstr (Call tc ty f args)   = ppCall tc ty f args
ppInstr (Alloca ty len align) = ppAlloca ty len align
ppInstr (ICmp op l r)         = text "icmp" <+> ppICmpOp op
                            <+> ppTyped ppValue l <> comma <+> ppValue r
ppInstr (FCmp op l r)         = text "fcmp" <+> ppFCmpOp op
                            <+> ppTyped ppValue l <> comma <+> ppValue r
ppInstr (Phi ty vls)          = text "phi" <+> ppType ty
                            <+> commas (map ppPhiArg vls)
ppInstr (Select c t f)        = text "select" <+> ppTyped ppValue c
                             <> comma <+> ppTyped ppValue t
                             <> comma <+> ppType (typedType t) <+> ppValue f
ppInstr (Comment str)         = char ';' <+> text str

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
  body = text "call" <+> ppType ty <+> ppValue f
      <> parens (commas (map (ppTyped ppValue) args))

ppPhiArg :: (Value,Ident) -> Doc
ppPhiArg (v,l) = brackets (ppValue v <> comma <+> ppIdent l)

data ICmpOp = Ieq | Ine | Iugt | Iuge | Iult | Iule | Isgt | Isge | Islt | Isle
  deriving (Show)

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

data FCmpOp = Ffalse  | Foeq | Fogt | Foge | Folt | Fole | Fone
            | Ford    | Fueq | Fugt | Fuge | Fult | Fule | Fune
            | Funo    | Ftrue
    deriving (Show)

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

-- Arguments -------------------------------------------------------------------

data Arg
  = TypedArg (Typed Value)
  | UntypedArg Value
  | TypeArg Type
    deriving (Show)

ppArg :: Arg -> Doc
ppArg (TypedArg tl)  = ppTyped ppValue tl
ppArg (UntypedArg l) = ppValue l
ppArg (TypeArg ty)   = ppType ty

-- Values ----------------------------------------------------------------------

data Value
  = ValInteger Integer
  | ValFloat Float
  | ValDouble Double
  | ValIdent Ident
  | ValSymbol Symbol
  | ValNull
    deriving (Show)

ppValue :: Value -> Doc
ppValue (ValInteger i) = integer i
ppValue (ValFloat i)   = float i
ppValue (ValDouble i)  = double i
ppValue (ValIdent i)   = ppIdent i
ppValue (ValSymbol s)  = ppSymbol s
ppValue ValNull        = text "null"

-- Statements ------------------------------------------------------------------

data Stmt
  = Result Ident Instr
  | Effect Instr
    deriving (Show)

stmtInstr :: Stmt -> Instr
stmtInstr (Result _ i) = i
stmtInstr (Effect i)   = i

ppStmt :: Stmt -> Doc
ppStmt (Result var i) = ppIdent var <+> char '=' <+> ppInstr i
ppStmt (Effect i)     = ppInstr i

ignore :: Instr -> Stmt
ignore  = Effect

-- Instruction Helpers ---------------------------------------------------------

comment :: String -> Instr
comment  = Comment

br :: Ident -> Instr
br l = GenInstr "br" [TypedArg (Typed (PrimType Label) (ValIdent l))]

condBr :: Value -> Ident -> Ident -> Instr
condBr b t f = GenInstr "br" [cond, label t, label f]
  where
  label = TypedArg . Typed (PrimType Label) . ValIdent
  cond  = TypedArg (Typed (PrimType (Integer 1)) b)

unreachable :: Instr
unreachable  = GenInstr "unreachable" []

unwind :: Instr
unwind  = GenInstr "unwind" []

alloca :: Type -> Maybe (Typed Value) -> Maybe Int -> Instr
alloca  = Alloca

load :: Typed Value -> Instr
load p = GenInstr "load" [TypedArg p]

store :: Typed Value -> Typed Value -> Instr
store a p = GenInstr "store" [TypedArg a, TypedArg p]

icmp :: ICmpOp -> Typed Value -> Value -> Instr
icmp  = ICmp

fcmp :: FCmpOp -> Typed Value -> Value -> Instr
fcmp  = FCmp

phi :: Type -> [(Value,Ident)] -> Instr
phi  = Phi

getelementptr :: Typed Value -> [Typed Value] -> Instr
getelementptr tv ixs = GenInstr "getelementptr" (TypedArg tv:map TypedArg ixs)
