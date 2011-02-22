module Text.LLVM.AST where

import Data.Int (Int32)
import Data.List (intersperse)
import Data.Monoid (Monoid(..))
import Text.PrettyPrint.HughesPJ

commas :: [Doc] -> Doc
commas ds = hcat (intersperse (comma <> space) ds)

int32 :: Int32 -> Doc
int32  = integer . fromIntegral

angles :: Doc -> Doc
angles d = char '<' <> d <> char '>'

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
ppModule m = hcat $ concat
  [ map ppTypeDecl (modTypes m)
  , map ppDeclare (modDeclares m)
  , map ppDefine (modDefines m)
  ]

-- Identifiers -----------------------------------------------------------------

newtype Ident = Ident String
    deriving (Show)

ppIdent :: Ident -> Doc
ppIdent (Ident n) = char '%' <> text n

-- Symbols ---------------------------------------------------------------------

newtype Symbol = Symbol String
    deriving Show

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
    deriving (Show)

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
    deriving (Show)

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
  | FunTy [Type] Type
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
ppType (FunTy as r)      = ppType r <> parens (commas (map ppType as))
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
ppDeclare d = text "declare" <+> ppType (decRetType d)
          <+> ppSymbol (decName d) <> parens (commas (map ppType (decArgs d)))

-- Function Definitions --------------------------------------------------------

data Define = Define
  { defRetType :: Type
  , defName    :: Symbol
  , defArgs    :: [Typed Ident]
  , defBody    :: [Stmt]
  } deriving (Show)

ppDefine :: Define -> Doc
ppDefine d = text "define" <+> ppType (defRetType d)
         <+> ppSymbol (defName d)
          <> parens (commas (map (ppTyped ppIdent) (defArgs d))) <+> char '{'
         $+$ nest 2 (vcat (map ppStmt (defBody d)))
         $+$ char '}'

-- Typed Things ----------------------------------------------------------------

data Typed a = Typed
  { typedType  :: Type
  , typedValue :: a
  } deriving Show

ppTyped :: (a -> Doc) -> Typed a -> Doc
ppTyped fmt ty = ppType (typedType ty) <+> fmt (typedValue ty)

-- Instructions ----------------------------------------------------------------

data Instr
  = GenInstr String [Arg]
  | Call Bool Type Symbol [Arg]
    deriving (Show)

ppInstr :: Instr -> Doc
ppInstr (GenInstr op args)    = text op <+> commas (map ppArg args)
ppInstr (Call tc ty sym args) = ppCall tc ty sym args

ppCall :: Bool -> Type -> Symbol -> [Arg] -> Doc
ppCall tc ty sym args
  | tc        = text "tail" <+> body
  | otherwise = body
  where
  body = text "call" <+> ppType ty <+> ppSymbol sym
      <> parens (commas (map ppArg args))

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
    deriving (Show)

ppValue :: Value -> Doc
ppValue (ValInteger i) = integer i
ppValue (ValFloat i)   = float i
ppValue (ValDouble i)  = double i
ppValue (ValIdent i)   = ppIdent i

-- Statements ------------------------------------------------------------------

data Stmt
  = Result Ident Instr
  | Effect Instr
    deriving (Show)

ppStmt :: Stmt -> Doc
ppStmt (Result var i) = ppIdent var <+> char '=' <+> ppInstr i
ppStmt (Effect i)     = ppInstr i

ignore :: Instr -> Stmt
ignore  = Effect

(=:) :: Ident -> Instr -> Stmt
(=:)  = Result

-- Instruction Helpers ---------------------------------------------------------

ret :: Typed Value -> Instr
ret v = GenInstr "ret" [TypedArg v]

retVoid :: Instr
retVoid  = GenInstr "ret" [TypeArg (PrimType Void)]

call :: Bool -> Type -> Symbol -> [Typed Value] -> Instr
call tc rty sym = Call tc rty sym . map TypedArg

add :: Typed Value -> Value -> Instr
add l r = GenInstr "add" [TypedArg l,UntypedArg r]

sub :: Typed Value -> Value -> Instr
sub l r = GenInstr "sub" [TypedArg l,UntypedArg r]

mul :: Typed Value -> Value -> Instr
mul l r = GenInstr "mul" [TypedArg l,UntypedArg r]

getelementptr :: Typed Value -> [(Type,Value)] -> Instr
getelementptr tv ixs = GenInstr "getelementptr" (TypedArg tv : args)
  where
  args | null ixs  = [TypedArg (Typed (PrimType (Integer 32)) (ValInteger 0))]
       | otherwise = map (TypedArg . uncurry Typed) ixs
