module Text.LLVM.AST where

import Data.Int (Int32)
import Data.List (intersperse)
import Text.PrettyPrint.HughesPJ

commas :: [Doc] -> Doc
commas ds = hcat (intersperse (comma <> space) ds)

-- Modules ---------------------------------------------------------------------

data Module = Module
  { modTypes    :: [TypeDecl]
  , modDeclares :: [Declare]
  , modDefines  :: [Define]
  } deriving (Show)

emptyModule :: Module
emptyModule  = Module
  { modTypes    = []
  , modDeclares = []
  , modDefines  = []
  }

ppModule :: Module -> Doc
ppModule m = hcat $ concat [ map ppTypeDecl (modTypes m)
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

data Type
  = Void
  | Alias Ident
  | Integer Int32
  | Float
  | Double
  | PtrTo Type
  | Label
  | FunTy [Type] Type
    deriving (Show)

ppType :: Type -> Doc
ppType Void         = text "void"
ppType (Alias i)    = ppIdent i
ppType (Integer i)  = char 'i' <> integer (toInteger i)
ppType Float        = text "float"
ppType Double       = text "double"
ppType (PtrTo ty)   = ppType ty <> char '*'
ppType Label        = text "label"
ppType (FunTy as r) = ppType r <> parens (commas (map ppType as))

-- Top-level Type Aliases ------------------------------------------------------

data TypeDecl = TypeDecl
  { typeName  :: Ident
  , typeValue :: Type
  } deriving (Show)

ppTypeDecl :: TypeDecl -> Doc
ppTypeDecl td = ppIdent (typeName td) <+> char '=' <+> ppType (typeValue td)

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
          <> parens (commas (map (ppTyped ppIdent) (defArgs d)))

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
    deriving (Show)

ppArg :: Arg -> Doc
ppArg (TypedArg tl)  = ppTyped ppValue tl
ppArg (UntypedArg l) = ppValue l

-- Values ----------------------------------------------------------------------

data Value
  = ValNum Integer
  | ValIdent Ident
    deriving (Show)

ppValue :: Value -> Doc
ppValue (ValNum i)   = integer i
ppValue (ValIdent i) = ppIdent i

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

call :: Type -> Symbol -> [Arg] -> Instr
call  = Call False

tailCall :: Type -> Symbol -> [Arg] -> Instr
tailCall  = Call True

add :: Type -> Value -> Value -> Instr
add ty l r = GenInstr "add" [TypedArg (Typed ty l),UntypedArg r]

sub :: Type -> Value -> Value -> Instr
sub ty l r = GenInstr "sub" [TypedArg (Typed ty l),UntypedArg r]

mul :: Type -> Value -> Value -> Instr
mul ty l r = GenInstr "mul" [TypedArg (Typed ty l),UntypedArg r]
