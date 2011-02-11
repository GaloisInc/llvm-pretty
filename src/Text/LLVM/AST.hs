module Text.LLVM.AST where

import Data.Int (Int32)
import Data.List (intersperse)
import Text.PrettyPrint.HughesPJ

commas :: [Doc] -> Doc
commas ds = hcat (intersperse (comma <> space) ds)

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
  , defArgs    :: [Argument]
  } deriving (Show)

ppDefine :: Define -> Doc
ppDefine d = text "define" <+> ppType (defRetType d)
         <+> ppSymbol (defName d)
          <> parens (commas (map ppArgument (defArgs d)))

-- Arguments -------------------------------------------------------------------

data Argument = Argument
  { argType :: Type
  , argName :: Ident
  } deriving (Show)

ppArgument :: Argument -> Doc
ppArgument arg = ppType (argType arg) <+> ppIdent (argName arg)
