{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternGuards #-}

module Text.LLVM.AST where

import Control.Applicative ((<$))
import Control.Monad (MonadPlus(mzero),(<=<),msum,guard,liftM,liftM3)
import Data.Char (isAscii,isPrint,ord,toUpper)
import Data.Foldable (Foldable(foldMap))
import Data.Int (Int32)
import Data.List (intersperse,genericIndex,genericLength,unfoldr)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.String (IsString(fromString))
import Data.Traversable (Traversable(sequenceA))
import Numeric (showHex)
import Text.PrettyPrint.HughesPJ


commas :: [Doc] -> Doc
commas  = hsep . punctuate (char ',')

colons :: [Doc] -> Doc
colons  = hcat . intersperse (char ':')

breaks :: (a -> Bool) -> [a] -> [[a]]
breaks p = unfoldr step
  where
  step [] = Nothing
  step xs = case break p xs of
    (as,_:bs) -> Just (as,bs)
    (as,  []) -> Just (as,[])

uncons :: MonadPlus m => [a] -> m (a,[a])
uncons (a:as) = return (a,as)
uncons _      = mzero

int32 :: Int32 -> Doc
int32  = integer . fromIntegral

angles :: Doc -> Doc
angles d = char '<' <> d <> char '>'

structBraces :: Doc -> Doc
structBraces body = char '{' <+> body <+> char '}'

ppMaybe :: (a -> Doc) -> Maybe a -> Doc
ppMaybe  = maybe empty

opt :: Bool -> Doc -> Doc
opt True  d = d
opt False _ = empty

-- Modules ---------------------------------------------------------------------

data Module = Module
  { modDataLayout :: DataLayout
  , modTypes      :: [TypeDecl]
  , modNamedMd    :: [NamedMd]
  , modUnnamedMd  :: [UnnamedMd]
  , modGlobals    :: [Global]
  , modDeclares   :: [Declare]
  , modDefines    :: [Define]
  , modInlineAsm  :: InlineAsm
  , modAliases    :: [GlobalAlias]
  } deriving (Show)

instance Monoid Module where
  mempty = emptyModule
  mappend m1 m2 = Module
    { modDataLayout = modDataLayout m1 `mappend` modDataLayout m2
    , modTypes      = modTypes      m1 `mappend` modTypes      m2
    , modUnnamedMd  = modUnnamedMd  m1 `mappend` modUnnamedMd  m2
    , modNamedMd    = modNamedMd    m1 `mappend` modNamedMd    m2
    , modGlobals    = modGlobals    m1 `mappend` modGlobals    m2
    , modDeclares   = modDeclares   m1 `mappend` modDeclares   m2
    , modDefines    = modDefines    m1 `mappend` modDefines    m2
    , modInlineAsm  = modInlineAsm  m1 `mappend` modInlineAsm  m2
    , modAliases    = modAliases    m1 `mappend` modAliases    m2
    }

emptyModule :: Module
emptyModule  = Module
  { modDataLayout = mempty
  , modTypes      = mempty
  , modNamedMd    = mempty
  , modUnnamedMd  = mempty
  , modGlobals    = mempty
  , modDeclares   = mempty
  , modDefines    = mempty
  , modInlineAsm  = mempty
  , modAliases    = mempty
  }

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

-- Named Metadata --------------------------------------------------------------

data NamedMd = NamedMd
  { nmName   :: String
  , nmValues :: [Int]
  } deriving (Show)

ppNamedMd :: NamedMd -> Doc
ppNamedMd nm = ppMetadata (text (nmName nm)) <+> char '='
  <+> ppMetadata (braces (commas (map (ppMetadata . int) (nmValues nm))))

-- Unnamed Metadata ------------------------------------------------------------

data UnnamedMd = UnnamedMd
  { umIndex  :: !Int
  , umValues :: [Typed Value]
  } deriving (Show)

ppUnnamedMd :: UnnamedMd -> Doc
ppUnnamedMd um = ppMetadata (int (umIndex um))
             <+> char '=' <+> text "metadata"
             <+> ppMetadataNode (umValues um)

-- Aliases ---------------------------------------------------------------------

data GlobalAlias = GlobalAlias
  { aliasName   :: Symbol
  , aliasType   :: Type
  , aliasTarget :: Value
  } deriving (Show)

ppGlobalAlias :: GlobalAlias -> Doc
ppGlobalAlias g = ppSymbol (aliasName g) <+> char '=' <+> body
  where
  val  = aliasTarget g
  body = case val of
    ValSymbol _sym -> ppType (aliasType g) <+> ppValue val
    _              -> ppValue val

-- Data Layout -----------------------------------------------------------------

type DataLayout = [LayoutSpec]

-- | Pretty print a data layout specification.
ppDataLayout :: DataLayout -> Doc
ppDataLayout [] = empty
ppDataLayout ls = text "target" <+> text "datalayout" <+> char '='
    <+> doubleQuotes (hcat (intersperse (char '-') (map ppLayoutSpec ls)))

data LayoutSpec
  = BigEndian
  | LittleEndian
  | PointerSize   !Int !Int (Maybe Int)
  | IntegerSize   !Int !Int (Maybe Int)
  | VectorSize    !Int !Int (Maybe Int)
  | FloatSize     !Int !Int (Maybe Int)
  | AggregateSize !Int !Int (Maybe Int)
  | StackObjSize  !Int !Int (Maybe Int)
  | NativeIntSize [Int]
  | StackAlign    !Int
    deriving (Show)

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

-- | Pretty-print the common case for data layout specifications.
ppLayoutBody :: Int -> Int -> Maybe Int -> Doc
ppLayoutBody size abi mb = int size <> char ':' <> int abi <> pref
  where
  pref = case mb of
    Nothing -> empty
    Just p  -> char ':' <> int p

-- | Parse the data layout string.
parseDataLayout :: MonadPlus m => String -> m DataLayout
parseDataLayout  = mapM parseLayoutSpec . breaks (== '-')

-- | Parse a single layout specification from a string.
parseLayoutSpec :: MonadPlus m => String -> m LayoutSpec
parseLayoutSpec str = msum
  [ guard (str == "E") >> return BigEndian
  , guard (str == "e") >> return LittleEndian
  , do (i,rest) <- uncons str
       let body = breaks (== ':') rest
       case i of

         'S' -> do align <- parseInt rest
                   return (StackAlign align)

         'p' -> build PointerSize (tail body)
         'i' -> build IntegerSize       body
         'v' -> build VectorSize        body
         'f' -> build FloatSize         body
         'a' -> build AggregateSize     body
         's' -> build StackObjSize      body

         'n' -> do ints <- mapM parseInt body
                   return (NativeIntSize ints)

         _   -> mzero
  ]

  where

  build f lst = case lst of
    [sz,abi,pref] -> liftM3 f (parseInt sz) (parseInt abi) (parsePref pref)
    [sz,abi]      -> liftM3 f (parseInt sz) (parseInt abi) (return Nothing)
    _             -> mzero

  parsePref = liftM Just . parseInt

  parseInt s = case reads s of
    [(i,[])] -> return i
    _        -> mzero

-- Inline Assembly -------------------------------------------------------------

type InlineAsm = [String]

-- | Pretty-print the inline assembly block.
ppInlineAsm :: InlineAsm -> Doc
ppInlineAsm  = foldr ($+$) empty . map ppLine
  where
  ppLine l = text "module asm" <+> doubleQuotes (text l)

-- Identifiers -----------------------------------------------------------------

newtype Ident = Ident String
    deriving (Show,Eq,Ord)

instance IsString Ident where
  fromString = Ident

ppIdent :: Ident -> Doc
ppIdent (Ident n) = char '%' <> text n

-- Symbols ---------------------------------------------------------------------

newtype Symbol = Symbol String
    deriving (Show,Eq,Ord)

instance IsString Symbol where
  fromString = Symbol

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
    deriving (Eq, Ord, Show)

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
    deriving (Eq, Ord, Show)

ppFloatType :: FloatType -> Doc
ppFloatType Float     = text "float"
ppFloatType Double    = text "double"
ppFloatType Fp128     = text "fp128"
ppFloatType X86_fp80  = text "x86_fp80"
ppFloatType PPC_fp128 = text "ppc_fp128"

type Type = Type' Ident

data Type' ident
  = PrimType PrimType
  | Alias ident
  | Array Int32 (Type' ident)
  | FunTy (Type' ident) [Type' ident] Bool
  | PtrTo (Type' ident)
  | Struct [Type' ident]
  | PackedStruct [Type' ident]
  | Vector Int32 (Type' ident)
  | Opaque
    deriving (Eq, Ord, Show, Functor)

-- | Traverse a type, updating or removing aliases.
updateAliases :: (a -> Type' b) -> (Type' a -> Type' b)
updateAliases f = loop
  where
  loop ty = case ty of
    Array len ety    -> Array len    (loop ety)
    FunTy res ps var -> FunTy        (loop res) (map loop ps) var
    PtrTo pty        -> PtrTo        (loop pty)
    Struct fs        -> Struct       (map loop fs)
    PackedStruct fs  -> PackedStruct (map loop fs)
    Alias lab        -> f lab
    PrimType pty     -> PrimType pty
    Vector len ety   -> Vector len (loop ety)
    Opaque           -> Opaque


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

isFloatingPoint :: PrimType -> Bool
isFloatingPoint (FloatType _) = True
isFloatingPoint _             = False

isAlias :: Type -> Bool
isAlias Alias{} = True
isAlias _       = False

isPrimTypeOf :: (PrimType -> Bool) -> Type -> Bool
isPrimTypeOf p (PrimType pt) = p pt
isPrimTypeOf _ _             = False

isLabel :: PrimType -> Bool
isLabel Label = True
isLabel _     = False

isInteger :: PrimType -> Bool
isInteger Integer{} = True
isInteger _         = False

isVector :: Type -> Bool
isVector Vector{} = True
isVector _        = False

isVectorOf :: (Type -> Bool) -> Type -> Bool
isVectorOf p (Vector _ e) = p e
isVectorOf _ _            = False

isArray :: Type -> Bool
isArray ty = case ty of
  Array _ _ -> True
  _         -> False

isPointer :: Type -> Bool
isPointer (PtrTo _) = True
isPointer _         = False

-- | Build a variable-argument argument list.
ppArgList :: Bool -> [Doc] -> Doc
ppArgList True  ds = parens (commas (ds ++ [text "..."]))
ppArgList False ds = parens (commas ds)

-- Null Values -----------------------------------------------------------------

data NullResult lab
  = HasNull (Value' lab)
  | ResolveNull Ident

primTypeNull :: PrimType -> Value' lab
primTypeNull (Integer 1)    = ValBool False
primTypeNull (Integer _)    = ValInteger 0
primTypeNull (FloatType ft) = floatTypeNull ft
primTypeNull _              = ValZeroInit

floatTypeNull :: FloatType -> Value' lab
floatTypeNull Float = ValFloat 0
floatTypeNull _     = ValDouble 0 -- XXX not sure about this

typeNull :: Type -> NullResult lab
typeNull (PrimType pt) = HasNull (primTypeNull pt)
typeNull PtrTo{}       = HasNull ValNull
typeNull (Alias i)     = ResolveNull i
typeNull _             = HasNull ValZeroInit

-- Type Elimination ------------------------------------------------------------

elimFunTy :: MonadPlus m => Type -> m (Type,[Type],Bool)
elimFunTy (FunTy ret args va) = return (ret,args,va)
elimFunTy _                   = mzero

elimAlias :: MonadPlus m => Type -> m Ident
elimAlias (Alias i) = return i
elimAlias _         = mzero

elimPtrTo :: MonadPlus m => Type -> m Type
elimPtrTo (PtrTo ty) = return ty
elimPtrTo _          = mzero

elimVector :: MonadPlus m => Type -> m (Int32,Type)
elimVector (Vector n pty) = return (n,pty)
elimVector _              = mzero

elimArray :: MonadPlus m => Type -> m (Int32, Type)
elimArray (Array n ety) = return (n, ety)
elimArray _             = mzero

elimFunPtr :: MonadPlus m => Type -> m (Type,[Type],Bool)
elimFunPtr  = elimFunTy <=< elimPtrTo

elimPrimType :: MonadPlus m => Type -> m PrimType
elimPrimType (PrimType pt) = return pt
elimPrimType _             = mzero

elimFloatType :: MonadPlus m => PrimType -> m FloatType
elimFloatType (FloatType ft) = return ft
elimFloatType _              = mzero

-- | Eliminator for array, pointer and vector types.
elimSequentialType :: MonadPlus m => Type -> m Type
elimSequentialType ty = case ty of
  Array _ elTy -> return elTy
  PtrTo elTy   -> return elTy
  Vector _ pty -> return pty
  _            -> mzero


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
  , globalAttrs :: GlobalAttrs
  , globalType  :: Type
  , globalValue :: Value
  , globalAlign :: Maybe Align
  } deriving Show

ppGlobal :: Global -> Doc
ppGlobal g = ppSymbol (globalSym g) <+> char '='
         <+> ppGlobalAttrs (globalAttrs g)
         <+> ppType (globalType g) <+> ppValue (globalValue g)
          <> ppAlign (globalAlign g)

addGlobal :: Global -> Module -> Module
addGlobal g m = m { modGlobals = g : modGlobals m }

data GlobalAttrs = GlobalAttrs
  { gaLinkage    :: Maybe Linkage
  , gaConstant   :: Bool
  } deriving (Show)

ppGlobalAttrs :: GlobalAttrs -> Doc
ppGlobalAttrs ga = ppMaybe ppLinkage (gaLinkage ga) <+> constant
  where
  constant | gaConstant ga = text "constant"
           | otherwise     = text "global"

-- Declarations ----------------------------------------------------------------

data Declare = Declare
  { decRetType :: Type
  , decName    :: Symbol
  , decArgs    :: [Type]
  , decVarArgs :: Bool
  } deriving (Show)

ppDeclare :: Declare -> Doc
ppDeclare d = text "declare"
          <+> ppType (decRetType d)
          <+> ppSymbol (decName d)
           <> ppArgList (decVarArgs d) (map ppType (decArgs d))

-- Function Definitions --------------------------------------------------------

data Define = Define
  { defAttrs   :: FunAttrs
  , defRetType :: Type
  , defName    :: Symbol
  , defArgs    :: [Typed Ident]
  , defVarArgs :: Bool
  , defBody    :: [BasicBlock]
  } deriving (Show)

ppDefine :: Define -> Doc
ppDefine d = text "define"
         <+> ppMaybe ppLinkage (funLinkage (defAttrs d))
         <+> ppType (defRetType d)
         <+> ppSymbol (defName d)
          <> ppArgList (defVarArgs d) (map (ppTyped ppIdent) (defArgs d))
         <+> ppMaybe (\gc -> text "gc" <+> ppGC gc) (funGC (defAttrs d))
         <+> char '{'
         $+$ vcat (map ppBasicBlock (defBody d))
         $+$ char '}'

addDefine :: Define -> Module -> Module
addDefine d m = m { modDefines = d : modDefines m }

data FunAttrs = FunAttrs
  { funLinkage :: Maybe Linkage
  , funGC      :: Maybe GC
  } deriving (Show)

emptyFunAttrs :: FunAttrs
emptyFunAttrs  = FunAttrs
  { funLinkage = Nothing
  , funGC      = Nothing
  }

-- Basic Block Labels ----------------------------------------------------------

data BlockLabel
  = Named Ident
  | Anon Int
    deriving (Eq,Ord,Show)

instance IsString BlockLabel where
  fromString str = Named (fromString str)

ppLabelDef :: BlockLabel -> Doc
ppLabelDef (Named (Ident l)) = text l <> char ':'
ppLabelDef (Anon i)          = char ';' <+> text "<label>:" <+> int i

ppLabel :: BlockLabel -> Doc
ppLabel (Named l) = ppIdent l
ppLabel (Anon i)  = char '%' <> int i

-- Basic Blocks ----------------------------------------------------------------

data BasicBlock' lab = BasicBlock
  { bbLabel :: lab
  , bbStmts :: [Stmt]
  } deriving (Show)

type BasicBlock = BasicBlock' (Maybe BlockLabel)

ppBasicBlock :: BasicBlock -> Doc
ppBasicBlock bb = ppMaybe ppLabelDef (bbLabel bb)
              $+$ nest 2 (vcat (map ppStmt (bbStmts bb)))

-- TODO: Consider making this function have type BasicBlock' lab -> [lab]
brTargets :: BasicBlock' lab -> [BlockLabel]
brTargets (BasicBlock _ (stmtInstr . last -> termInst)) =
  case termInst of
    Br _ t1 t2         -> [t1, t2]
    Invoke _ _ _ to uw -> [to, uw]
    Jump t             -> [t]
    _                  -> []

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
  | External
  | DLLImport
  | DLLExport
    deriving (Eq,Show)

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

newtype GC = GC
  { getGC :: String
  } deriving (Show)

ppGC :: GC -> Doc
ppGC  = doubleQuotes . text . getGC

-- Typed Things ----------------------------------------------------------------

data Typed a = Typed
  { typedType  :: Type
  , typedValue :: a
  } deriving (Show,Functor)

instance Foldable Typed where
  foldMap f t = f (typedValue t)

instance Traversable Typed where
  sequenceA t = mk `fmap` typedValue t
    where
    mk b = t { typedValue = b }

mapMTyped :: Monad m => (a -> m b) -> Typed a -> m (Typed b)
mapMTyped f t = do
  b <- f (typedValue t)
  return t { typedValue = b }

ppTyped :: (a -> Doc) -> Typed a -> Doc
ppTyped fmt ty = ppType (typedType ty) <+> fmt (typedValue ty)

-- Instructions ----------------------------------------------------------------

data ArithOp
  = Add Bool Bool | FAdd
  | Sub Bool Bool | FSub
  | Mul Bool Bool | FMul
  | UDiv Bool     | SDiv Bool | FDiv
  | URem          | SRem      | FRem
    deriving (Eq,Show)

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

isIArith :: ArithOp -> Bool
isIArith Add{}  = True
isIArith Sub{}  = True
isIArith Mul{}  = True
isIArith UDiv{} = True
isIArith SDiv{} = True
isIArith URem   = True
isIArith SRem   = True
isIArith _      = False

isFArith :: ArithOp -> Bool
isFArith  = not . isIArith

data BitOp
  = Shl Bool Bool | Lshr Bool | Ashr Bool
  | And           | Or   | Xor
    deriving Show

ppBitOp :: BitOp -> Doc
ppBitOp (Shl nuw nsw) = text "shl"  <+> ppSignBits nuw nsw
ppBitOp (Lshr e)      = text "lshr" <+> ppExact e
ppBitOp (Ashr e)      = text "ashr" <+> ppExact e
ppBitOp And           = text "and"
ppBitOp Or            = text "or"
ppBitOp Xor           = text "xor"

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

type Align = Int

data Instr' lab
  = Ret (Typed (Value' lab))
  | RetVoid
  | Arith ArithOp (Typed (Value' lab)) (Value' lab)
  | Bit BitOp (Typed (Value' lab)) (Value' lab)
  | Conv ConvOp (Typed (Value' lab)) Type
  | Call Bool Type (Value' lab) [Typed (Value' lab)]
  | Alloca Type (Maybe (Typed (Value' lab))) (Maybe Int)
  | Load (Typed (Value' lab)) (Maybe Align)
  | Store (Typed (Value' lab)) (Typed (Value' lab)) (Maybe Align)
  | ICmp ICmpOp (Typed (Value' lab)) (Value' lab)
  | FCmp FCmpOp (Typed (Value' lab)) (Value' lab)
  | Phi Type [((Value' lab),lab)]
  | GEP Bool (Typed (Value' lab)) [Typed (Value' lab)]
  | Select (Typed (Value' lab)) (Typed (Value' lab)) (Value' lab)
  | ExtractValue (Typed (Value' lab)) [Int32]
  | InsertValue (Typed (Value' lab)) (Typed (Value' lab)) [Int32]
  | ExtractElt (Typed (Value' lab)) (Value' lab)
  | InsertElt (Typed (Value' lab)) (Typed (Value' lab)) (Value' lab)
  | ShuffleVector (Typed (Value' lab)) (Value' lab) (Typed (Value' lab))
  | Jump lab
  | Br (Typed (Value' lab)) lab lab
  | Invoke Type (Value' lab) [Typed (Value' lab)] lab lab
  | Comment String
  | Unreachable
  | Unwind
  | VaArg (Typed (Value' lab)) Type
  | IndirectBr (Typed (Value' lab)) [lab]
  | Switch (Typed (Value' lab)) lab [(Integer,lab)]
  | LandingPad Type (Typed (Value' lab)) Bool [Clause' lab]
  | Resume (Typed (Value' lab))
    deriving (Show,Functor)

type Instr = Instr' BlockLabel

data Clause' lab
  = Catch  (Typed (Value' lab))
  | Filter (Typed (Value' lab))
    deriving (Show,Functor)

type Clause = Clause' BlockLabel


isTerminator :: Instr' lab -> Bool
isTerminator instr = case instr of
  Ret{}        -> True
  RetVoid      -> True
  Jump{}       -> True
  Br{}         -> True
  Unreachable  -> True
  Unwind       -> True
  Invoke{}     -> True
  IndirectBr{} -> True
  Switch{}     -> True
  Resume{}     -> True
  _            -> False

isComment :: Instr' lab -> Bool
isComment Comment{} = True
isComment _         = False

isPhi :: Instr' lab -> Bool
isPhi Phi{} = True
isPhi _     = False

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
  Load ptr ma            -> text "load" <+> ppTyped ppValue ptr
                         <> ppAlign ma
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

-- Values ----------------------------------------------------------------------

data Value' lab
  = ValInteger Integer
  | ValBool Bool
  | ValFloat Float
  | ValDouble Double
  | ValIdent Ident
  | ValSymbol Symbol
  | ValNull
  | ValArray Type [Value' lab]
  | ValVector Type [Value' lab]
  | ValStruct [Typed (Value' lab)]
  | ValPackedStruct [Typed (Value' lab)]
  | ValString String
  | ValConstExpr (ConstExpr' lab)
  | ValUndef
  | ValLabel lab
  | ValZeroInit
  | ValAsm Bool Bool String String
  | ValMd (ValMd' lab)
    deriving (Show,Functor)

type Value = Value' BlockLabel

data ValMd' lab
  = ValMdString String
  | ValMdNode [Typed (Value' lab)]
  | ValMdRef Int
  | ValMdLoc (DebugLoc' lab)
    deriving (Show,Functor)

type ValMd = ValMd' BlockLabel

data DebugLoc' lab = DebugLoc
  { dlLine  :: Int32
  , dlCol   :: Int32
  , dlScope :: ValMd' lab
  , dlIA    :: Maybe (ValMd' lab)
  } deriving (Show,Functor)

type DebugLoc = DebugLoc' BlockLabel

isConst :: Value' lab -> Bool
isConst ValInteger{}   = True
isConst ValBool{}      = True
isConst ValFloat{}     = True
isConst ValDouble{}    = True
isConst ValConstExpr{} = True
isConst ValZeroInit    = True
isConst ValNull        = True
isConst _              = False

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
  ValMdNode vs    -> ppMetadataNode vs
  ValMdRef i      -> ppMetadata (int i)
  ValMdLoc l      -> ppDebugLoc l

ppDebugLoc :: DebugLoc -> Doc
ppDebugLoc dl = ppMetadata $ structBraces $ commas
  [ ppType (PrimType (Integer 32)) <+> int32 (dlLine dl)
  , ppType (PrimType (Integer 32)) <+> int32 (dlCol dl)
  , ppTypedValMd (dlScope dl)
  , maybe (text "null") ppTypedValMd (dlIA dl)
  ]

ppTypedValMd :: ValMd -> Doc
ppTypedValMd  = ppTyped ppValMd . Typed (PrimType Metadata)

ppMetadata :: Doc -> Doc
ppMetadata body = char '!' <> body

ppMetadataNode :: [Typed Value] -> Doc
ppMetadataNode vs = ppMetadata (braces (commas (map (ppTyped ppValue) vs)))

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

-- Value Elimination -----------------------------------------------------------

elimValSymbol :: MonadPlus m => Value' lab -> m Symbol
elimValSymbol (ValSymbol sym) = return sym
elimValSymbol _               = mzero

elimValInteger :: MonadPlus m => Value' lab -> m Integer
elimValInteger (ValInteger i) = return i
elimValInteger _              = mzero

-- Statements ------------------------------------------------------------------

data Stmt' lab
  = Result Ident (Instr' lab) [(String,ValMd' lab)]
  | Effect (Instr' lab) [(String,ValMd' lab)]
    deriving (Show,Functor)

type Stmt = Stmt' BlockLabel

stmtInstr :: Stmt' lab -> Instr' lab
stmtInstr (Result _ i _) = i
stmtInstr (Effect i _)   = i

stmtMetadata :: Stmt' lab -> [(String,ValMd' lab)]
stmtMetadata stmt = case stmt of
  Result _ _ mds -> mds
  Effect _ mds   -> mds

extendMetadata :: (String,ValMd' lab) -> Stmt' lab -> Stmt' lab
extendMetadata md stmt = case stmt of
  Result r i mds -> Result r i (md:mds)
  Effect i mds   -> Effect i (md:mds)

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


-- Constant Expressions --------------------------------------------------------

data ConstExpr' lab
  = ConstGEP Bool [Typed (Value' lab)]
  | ConstConv ConvOp (Typed (Value' lab)) Type
  | ConstSelect (Typed (Value' lab)) (Typed (Value' lab)) (Typed (Value' lab))
  | ConstBlockAddr Symbol lab
    deriving (Show,Functor)

type ConstExpr = ConstExpr' BlockLabel

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


-- Aggregate Utilities ---------------------------------------------------------

data IndexResult
  = Invalid                             -- ^ An invalid use of GEP
  | HasType Type                        -- ^ A resolved type
  | Resolve Ident (Type -> IndexResult) -- ^ Continue, after resolving an alias

isInvalid :: IndexResult -> Bool
isInvalid ir = case ir of
  Invalid -> True
  _       -> False

-- | Resolve the type of a GEP instruction.  Note that the type produced is the
-- type of the result, not necessarily a pointer.
resolveGep :: Type -> [Typed (Value' lab)] -> IndexResult
resolveGep (PtrTo ty0) (v:ixs0)
  | isGepIndex v =
    resolveGepBody ty0 ixs0
resolveGep ty0@PtrTo{} (v:ixs0)
  | Just i <- elimAlias (typedType v) =
    Resolve i (\ty' -> resolveGep ty0 (Typed ty' (typedValue v):ixs0))
resolveGep (Alias i) ixs =
    Resolve i (\ty' -> resolveGep ty' ixs)
resolveGep _ _ = Invalid

-- | Resolve the type of a GEP instruction.  This assumes that the input has
-- already been processed as a pointer.
resolveGepBody :: Type -> [Typed (Value' lab)] -> IndexResult
resolveGepBody (Struct fs) (v:ixs)
  | Just i <- isGepStructIndex v, genericLength fs > i =
    resolveGepBody (genericIndex fs i) ixs
resolveGepBody (PackedStruct fs) (v:ixs)
  | Just i <- isGepStructIndex v, genericLength fs > i =
    resolveGepBody (genericIndex fs i) ixs
resolveGepBody (Alias name) is
  | not (null is) =
    Resolve name (\ty' -> resolveGepBody ty' is)
resolveGepBody (Array _ ty') (v:ixs)
  | isGepIndex v =
    resolveGepBody ty' ixs
resolveGepBody (Vector _ tp) [val]
  | isGepIndex val =
    HasType tp
resolveGepBody ty (v:ixs)
  | Just i <- elimAlias (typedType v) =
    Resolve i (\ty' -> resolveGepBody ty (Typed ty' (typedValue v):ixs))
resolveGepBody ty [] =
    HasType ty
resolveGepBody _ _ =
    Invalid

isGepIndex :: Typed (Value' lab) -> Bool
isGepIndex tv = isPrimTypeOf isInteger (typedType tv)

isGepStructIndex :: Typed (Value' lab) -> Maybe Integer
isGepStructIndex tv = do
  guard (isGepIndex tv)
  elimValInteger (typedValue tv)

resolveValueIndex :: Type -> [Int32] -> IndexResult
resolveValueIndex ty is@(ix:ixs) = case ty of
  Struct fs | genericLength fs > ix
    -> resolveValueIndex (genericIndex fs ix) ixs

  PackedStruct fs | genericLength fs > ix
    -> resolveValueIndex (genericIndex fs ix) ixs

  Array n ty' | fromIntegral ix < n
    -> resolveValueIndex ty' ixs

  Alias name
    -> Resolve name (\ty' -> resolveValueIndex ty' is)

  _ -> Invalid
resolveValueIndex ty [] = HasType ty
