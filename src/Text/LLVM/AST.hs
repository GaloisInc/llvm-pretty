{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveGeneric #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

module Text.LLVM.AST where

import Data.Functor.Identity (Identity(..))
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Control.Monad (MonadPlus(mzero,mplus),(<=<),guard)
import Data.Int (Int32,Int64)
import Data.List (genericIndex,genericLength)
import qualified Data.Map as Map
import Data.Semigroup as Sem
import Data.String (IsString(fromString))
import Data.Word (Word8,Word16,Word32,Word64)
import GHC.Generics (Generic, Generic1)

import Text.Parsec
import Text.Parsec.String

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<$))
import Data.Foldable (Foldable(foldMap))
import Data.Monoid (Monoid(..))
import Data.Traversable (Traversable(sequenceA))
#endif


-- Modules ---------------------------------------------------------------------

data Module = Module
  { modSourceName :: Maybe String
  , modDataLayout :: DataLayout    -- ^ type size and alignment information
  , modTypes      :: [TypeDecl]    -- ^ top-level type aliases
  , modNamedMd    :: [NamedMd]
  , modUnnamedMd  :: [UnnamedMd]
  , modComdat     :: Map.Map String SelectionKind
  , modGlobals    :: [Global]      -- ^ global value declarations
  , modDeclares   :: [Declare]     -- ^ external function declarations (without definitions)
  , modDefines    :: [Define]      -- ^ internal function declarations (with definitions)
  , modInlineAsm  :: InlineAsm
  , modAliases    :: [GlobalAlias]
  } deriving (Data, Eq, Ord, Generic, Show, Typeable)

instance Sem.Semigroup Module where
  m1 <> m2 = Module
    { modSourceName = modSourceName m1 `mplus`   modSourceName m2
    , modDataLayout = modDataLayout m1 <> modDataLayout m2
    , modTypes      = modTypes      m1 <> modTypes      m2
    , modUnnamedMd  = modUnnamedMd  m1 <> modUnnamedMd  m2
    , modNamedMd    = modNamedMd    m1 <> modNamedMd    m2
    , modGlobals    = modGlobals    m1 <> modGlobals    m2
    , modDeclares   = modDeclares   m1 <> modDeclares   m2
    , modDefines    = modDefines    m1 <> modDefines    m2
    , modInlineAsm  = modInlineAsm  m1 <> modInlineAsm  m2
    , modAliases    = modAliases    m1 <> modAliases    m2
    , modComdat     = modComdat     m1 <> modComdat     m2
    }

instance Monoid Module where
  mempty = emptyModule
  mappend m1 m2 = m1 <> m2

emptyModule :: Module
emptyModule  = Module
  { modSourceName = mempty
  , modDataLayout = mempty
  , modTypes      = mempty
  , modNamedMd    = mempty
  , modUnnamedMd  = mempty
  , modGlobals    = mempty
  , modDeclares   = mempty
  , modDefines    = mempty
  , modInlineAsm  = mempty
  , modAliases    = mempty
  , modComdat     = mempty
  }


-- Named Metadata --------------------------------------------------------------

data NamedMd = NamedMd
  { nmName   :: String
  , nmValues :: [Int]
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)


-- Unnamed Metadata ------------------------------------------------------------

data UnnamedMd = UnnamedMd
  { umIndex    :: !Int
  , umValues   :: ValMd
  , umDistinct :: Bool
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)


-- Aliases ---------------------------------------------------------------------

data GlobalAlias = GlobalAlias
  { aliasName   :: Symbol
  , aliasType   :: Type
  , aliasTarget :: Value
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)


-- Data Layout -----------------------------------------------------------------

type DataLayout = [LayoutSpec]

data LayoutSpec
  = BigEndian
  | LittleEndian
  | PointerSize   !Int !Int !Int (Maybe Int) -- ^ address space, size, abi, pref
  | IntegerSize   !Int !Int (Maybe Int) -- ^ size, abi, pref
  | VectorSize    !Int !Int (Maybe Int) -- ^ size, abi, pref
  | FloatSize     !Int !Int (Maybe Int) -- ^ size, abi, pref
  | StackObjSize  !Int !Int (Maybe Int) -- ^ size, abi, pref
  | AggregateSize !Int !Int (Maybe Int) -- ^ size, abi, pref
  | NativeIntSize [Int]
  | StackAlign    !Int -- ^ size
  | Mangling Mangling
    deriving (Data, Eq, Generic, Ord, Show, Typeable)

data Mangling = ElfMangling
              | MipsMangling
              | MachOMangling
              | WindowsCoffMangling
                deriving (Data, Eq, Enum, Generic, Ord, Show, Typeable)

-- | Parse the data layout string.
parseDataLayout :: MonadPlus m => String -> m DataLayout
parseDataLayout str =
  case parse (pDataLayout <* eof) "<internal>" str of
    Left _err -> mzero
    Right specs -> return specs
  where
    pDataLayout :: Parser DataLayout
    pDataLayout = sepBy pLayoutSpec (char '-')

    pLayoutSpec :: Parser LayoutSpec
    pLayoutSpec =
      do c <- letter
         case c of
           'E' -> return BigEndian
           'e' -> return LittleEndian
           'S' -> StackAlign    <$> pInt
           'p' -> PointerSize   <$> pInt0 <*> pCInt <*> pCInt <*> pPref
           'i' -> IntegerSize   <$> pInt <*> pCInt <*> pPref
           'v' -> VectorSize    <$> pInt <*> pCInt <*> pPref
           'f' -> FloatSize     <$> pInt <*> pCInt <*> pPref
           's' -> StackObjSize  <$> pInt <*> pCInt <*> pPref
           'a' -> AggregateSize <$> pInt <*> pCInt <*> pPref
           'n' -> NativeIntSize <$> sepBy pInt (char ':')
           'm' -> Mangling      <$> (char ':' >> pMangling)
           _   -> mzero

    pMangling :: Parser Mangling
    pMangling =
      do c <- letter
         case c of
           'e' -> return ElfMangling
           'm' -> return MipsMangling
           'o' -> return MachOMangling
           'w' -> return WindowsCoffMangling
           _   -> mzero

    pInt :: Parser Int
    pInt = read <$> many1 digit

    pInt0 :: Parser Int
    pInt0 = pInt <|> return 0

    pCInt :: Parser Int
    pCInt = char ':' >> pInt

    pPref :: Parser (Maybe Int)
    pPref = optionMaybe pCInt

-- Inline Assembly -------------------------------------------------------------

type InlineAsm = [String]

-- Comdat ----------------------------------------------------------------------

data SelectionKind = ComdatAny
                   | ComdatExactMatch
                   | ComdatLargest
                   | ComdatNoDuplicates
                   | ComdatSameSize
    deriving (Data, Eq, Enum, Generic, Ord, Show, Typeable)

-- Identifiers -----------------------------------------------------------------

newtype Ident = Ident String
    deriving (Data, Eq, Generic, Ord, Show, Typeable)

instance IsString Ident where
  fromString = Ident

-- Symbols ---------------------------------------------------------------------

newtype Symbol = Symbol String
    deriving (Data, Eq, Generic, Ord, Show, Typeable)

instance Sem.Semigroup Symbol where
  Symbol a <> Symbol b = Symbol (a <> b)

instance Monoid Symbol where
  mappend = (<>)
  mempty  = Symbol mempty

instance IsString Symbol where
  fromString = Symbol

-- Types -----------------------------------------------------------------------

data PrimType
  = Label
  | Void
  | Integer Int32
  | FloatType FloatType
  | X86mmx
  | Metadata
    deriving (Data, Eq, Generic, Ord, Show, Typeable)

data FloatType
  = Half
  | Float
  | Double
  | Fp128
  | X86_fp80
  | PPC_fp128
    deriving (Data, Eq, Enum, Generic, Ord, Show, Typeable)

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
    deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

-- | Applicatively traverse a type, updating or removing aliases.
updateAliasesA :: (Applicative f) => (a -> f (Type' b)) -> Type' a -> f (Type' b)
updateAliasesA f = loop
  where
  loop ty = case ty of
    Array len ety    -> Array len    <$> (loop ety)
    FunTy res ps var -> FunTy        <$> (loop res) <*> (traverse loop ps) <*> pure var
    PtrTo pty        -> PtrTo        <$> (loop pty)
    Struct fs        -> Struct       <$> (traverse loop fs)
    PackedStruct fs  -> PackedStruct <$> (traverse loop fs)
    Vector len ety   -> Vector       <$> pure len <*> (loop ety)
    PrimType pty     -> pure $ PrimType pty
    Opaque           -> pure $ Opaque
    Alias lab        -> f lab

-- | Traverse a type, updating or removing aliases.
updateAliases :: (a -> Type' b) -> Type' a -> Type' b
updateAliases f = coerce $ updateAliasesA (Identity . f)

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


-- Null Values -----------------------------------------------------------------

data NullResult lab
  = HasNull (Value' lab)
  | ResolveNull Ident
    deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

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
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)


-- Globals ---------------------------------------------------------------------

data Global = Global
  { globalSym      :: Symbol
  , globalAttrs    :: GlobalAttrs
  , globalType     :: Type
  , globalValue    :: Maybe Value
  , globalAlign    :: Maybe Align
  , globalMetadata :: GlobalMdAttachments
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

addGlobal :: Global -> Module -> Module
addGlobal g m = m { modGlobals = g : modGlobals m }

data GlobalAttrs = GlobalAttrs
  { gaLinkage    :: Maybe Linkage
  , gaVisibility :: Maybe Visibility
  , gaConstant   :: Bool
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

emptyGlobalAttrs :: GlobalAttrs
emptyGlobalAttrs  = GlobalAttrs
  { gaLinkage    = Nothing
  , gaVisibility = Nothing
  , gaConstant   = False
  }


-- Declarations ----------------------------------------------------------------

data Declare = Declare
  { decRetType :: Type
  , decName    :: Symbol
  , decArgs    :: [Type]
  , decVarArgs :: Bool
  , decAttrs   :: [FunAttr]
  , decComdat  :: Maybe String
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

-- | The function type of this declaration
decFunType :: Declare -> Type
decFunType Declare { .. } = PtrTo (FunTy decRetType decArgs decVarArgs)


-- Function Definitions --------------------------------------------------------

data Define = Define
  { defLinkage  :: Maybe Linkage
  , defRetType  :: Type
  , defName     :: Symbol
  , defArgs     :: [Typed Ident]
  , defVarArgs  :: Bool
  , defAttrs    :: [FunAttr]
  , defSection  :: Maybe String
  , defGC       :: Maybe GC
  , defBody     :: [BasicBlock]
  , defMetadata :: FnMdAttachments
  , defComdat   :: Maybe String
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

defFunType :: Define -> Type
defFunType Define { .. } =
  PtrTo (FunTy defRetType (map typedType defArgs) defVarArgs)

addDefine :: Define -> Module -> Module
addDefine d m = m { modDefines = d : modDefines m }

-- Function Attributes and attribute groups ------------------------------------


data FunAttr
   = AlignStack Int
   | Alwaysinline
   | Builtin
   | Cold
   | Inlinehint
   | Jumptable
   | Minsize
   | Naked
   | Nobuiltin
   | Noduplicate
   | Noimplicitfloat
   | Noinline
   | Nonlazybind
   | Noredzone
   | Noreturn
   | Nounwind
   | Optnone
   | Optsize
   | Readnone
   | Readonly
   | ReturnsTwice
   | SanitizeAddress
   | SanitizeMemory
   | SanitizeThread
   | SSP
   | SSPreq
   | SSPstrong
   | UWTable
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

-- Basic Block Labels ----------------------------------------------------------

data BlockLabel
  = Named Ident
  | Anon Int
    deriving (Data, Eq, Generic, Ord, Show, Typeable)

instance IsString BlockLabel where
  fromString str = Named (fromString str)

-- Basic Blocks ----------------------------------------------------------------

data BasicBlock' lab = BasicBlock
  { bbLabel :: Maybe lab
  , bbStmts :: [Stmt' lab]
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type BasicBlock = BasicBlock' BlockLabel

brTargets :: BasicBlock' lab -> [lab]
brTargets (BasicBlock _ stmts) =
  case stmtInstr (last stmts) of
    Br _ t1 t2         -> [t1, t2]
    Invoke _ _ _ to uw -> [to, uw]
    Jump t             -> [t]
    Switch _ l ls      -> l : map snd ls
    IndirectBr _ ls    -> ls
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
    deriving (Data, Eq, Enum, Generic, Ord, Show, Typeable)

data Visibility = DefaultVisibility
                | HiddenVisibility
                | ProtectedVisibility
    deriving (Data, Eq, Generic, Ord, Show, Typeable)

newtype GC = GC
  { getGC :: String
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

-- Typed Things ----------------------------------------------------------------

data Typed a = Typed
  { typedType  :: Type
  , typedValue :: a
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

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

-- Instructions ----------------------------------------------------------------

data ArithOp
  = Add Bool Bool
    {- ^ * Integral addition.
         * First boolean flag: check for unsigned overflow.
         * Second boolean flag: check for signed overflow.
         * If the checks fail, then the result is poisoned. -}
  | FAdd
    -- ^ Floating point addition.

  | Sub Bool Bool
    {- ^ * Integral subtraction.
         * First boolean flag: check for unsigned overflow.
         * Second boolean flag: check for signed overflow.
         * If the checks fail, then the result is poisoned. -}

  | FSub
    -- ^ Floating point subtraction.

  | Mul Bool Bool
    {- ^ * Integral multiplication.
         * First boolean flag: check for unsigned overflow.
         * Second boolean flag: check for signed overflow.
         * If the checks fail, then the result is poisoned. -}

  | FMul
    -- ^ Floating point multiplication.

  | UDiv Bool
    {- ^ * Integral unsigned division.
         * Boolean flag: check for exact result.
         * If the check fails, then the result is poisoned. -}

  | SDiv Bool
    {- ^ * Integral signed division.
         * Boolean flag: check for exact result.
         * If the check fails, then the result is poisoned. -}

  | FDiv
    -- ^ Floating point division.

  | URem
    -- ^ Integral unsigned reminder resulting from unsigned division.
    -- Division by 0 is undefined.

  | SRem
    -- ^ * Integral signded reminder resulting from signed division.
    --   * The sign of the reminder matches the divident (first parameter).
    --   * Division by 0 is undefined.

  | FRem
    -- ^ * Floating point reminder resulting from floating point division.
    --   * The reminder has the same sign as the divident (first parameter).

    deriving (Data, Eq, Generic, Ord, Show, Typeable)

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

-- | Binary bitwise operators.
data BitOp
  = Shl Bool Bool
    {- ^ * Shift left.
         * First bool flag: check for unsigned overflow (i.e., shifted out a 1).
         * Second bool flag: check for signed overflow
              (i.e., shifted out something that does not match the sign bit)

         If a check fails, then the result is poisoned.

         The value of the second parameter must be strictly less than the
           number of bits in the first parameter,
           otherwise the result is undefined.  -}

  | Lshr Bool
    {- ^ * Logical shift right.
         * The boolean is for exact check: poison the result,
              if we shift out a 1 bit (i.e., had to round).

    The value of the second parameter must be strictly less than the
    number of bits in the first parameter, otherwise the result is undefined.
    -}

  | Ashr Bool
    {- ^ * Arithmetic shift right.
         * The boolean is for exact check: poison the result,
                if we shift out a 1 bit (i.e., had to round).

    The value of the second parameter must be strictly less than the
    number of bits in the first parameter, otherwise the result is undefined.
    -}

  | And
  | Or
  | Xor
    deriving (Data, Eq, Generic, Ord, Show, Typeable)

-- | Conversions from one type to another.
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
    deriving (Data, Eq, Enum, Generic, Ord, Show, Typeable)

data AtomicRWOp
  = AtomicXchg
  | AtomicAdd
  | AtomicSub
  | AtomicAnd
  | AtomicNand
  | AtomicOr
  | AtomicXor
  | AtomicMax
  | AtomicMin
  | AtomicUMax
  | AcomicUMin
    deriving (Data, Eq, Enum, Generic, Ord, Show, Typeable)

data AtomicOrdering
  = Unordered
  | Monotonic
  | Acquire
  | Release
  | AcqRel
  | SeqCst
    deriving (Data, Eq, Enum, Generic, Ord, Show, Typeable)

type Align = Int

data Instr' lab
  = Ret (Typed (Value' lab))
    {- ^ * Return from function with the given value.
         * Ends basic block. -}

  | RetVoid
    {- ^ * Return from function.
         * Ends basic block. -}

  | Arith ArithOp (Typed (Value' lab)) (Value' lab)
    {- ^ * Binary arithmetic operation, both operands have the same type.
         * Middle of basic block.
         * The result is the same as parameters. -}

  | Bit BitOp (Typed (Value' lab)) (Value' lab)
    {- ^ * Binary bit-vector operation, both operands have the same type.
         * Middle of basic block.
         * The result is the same as parameters. -}

  | Conv ConvOp (Typed (Value' lab)) Type
    {- ^ * Convert a value from one type to another.
         * Middle of basic block.
         * The result matches the 3rd parameter. -}

  | Call Bool Type (Value' lab) [Typed (Value' lab)]
    {- ^ * Call a function.
            The boolean is tail-call hint (XXX: needs to be updated)
         * Middle of basic block.
         * The result is as indicated by the provided type. -}

  | Alloca Type (Maybe (Typed (Value' lab))) (Maybe Int)
    {- ^ * Allocated space on the stack:
           type of elements;
           how many elements (1 if 'Nothing');
           required alignment.
         * Middle of basic block.
         * Returns a pointer to hold the given number of elements. -}

  | Load (Typed (Value' lab)) (Maybe AtomicOrdering) (Maybe Align)
    {- ^ * Read a value from the given address:
           address to read from;
           assumptions about alignment of the given pointer.
         * Middle of basic block.
         * Returns a value of type matching the pointer. -}

  | Store (Typed (Value' lab)) (Typed (Value' lab)) (Maybe Align)
    {- ^ * Write a value to memory:
             value to store;
             pointer to location where to store;
             assumptions about the alignment of the given pointer.
         * Middle of basic block.
         * Effect. -}


  | Fence (Maybe String) AtomicOrdering
    {- ^ * Introduce a happens-before relationship between operations:
             synchronization scope;
             type of ordering.
         * Middle of basic block. -}

  | CmpXchg Bool Bool (Typed (Value' lab)) (Typed (Value' lab)) (Typed (Value' lab)) (Maybe String) AtomicOrdering AtomicOrdering
    {- ^ * Atomically compare and maybe exchange values in memory:
             whether the exchange is weak;
             whether the exchange is volatile;
             pointer to read;
             value to compare it with;
             new value to write if the two prior values are equal;
             synchronization scope;
             synchronization ordering on success;
             synchronization ordering on failure.
         * Returns a pair of the original value and whether an exchange occurred.
         * Middle of basic block.
         * Effect. -}

  | AtomicRW Bool AtomicRWOp (Typed (Value' lab)) (Typed (Value' lab)) (Maybe String) AtomicOrdering
    {- ^ * Perform an atomic load, operation, and store:
             whether the operation is volatile;
             operation to apply to the read value and the provided value;
             pointer to read;
             value to combine it with, using the given operation;
             synchronization scope;
             synchronization ordering.
         * Returns the original value at the given location.
         * Middle of basic block.
         * Effect. -}

  | ICmp ICmpOp (Typed (Value' lab)) (Value' lab)
    {- ^ * Compare two integral values.
         * Middle of basic block.
         * Returns a boolean value. -}

  | FCmp FCmpOp (Typed (Value' lab)) (Value' lab)
    {- ^ * Compare two floating point values.
         * Middle of basic block.
         * Returns a boolean value. -}

  | Phi Type [(Value' lab,lab)]
    {- ^ * Join point for an SSA value: we get one value per predecessor
           basic block.
         * Middle of basic block.
         * Returns a value of the specified type. -}

  | GEP Bool (Typed (Value' lab)) [Typed (Value' lab)]
    {- ^ * "Get element pointer",
            compute the address of a field in a structure:
            inbounds check (value poisoned if this fails);
            pointer to parent strucutre;
            path to a sub-component of a strucutre.
         * Middle of basic block.
         * Returns the address of the requiested member.

    The types in path are the types of the index, not the fields.

    The indexes are in units of a fields (i.e., the first element in
    a struct is field 0, the next one is 1, etc., regardless of the size
    of the fields in bytes). -}

  | Select (Typed (Value' lab)) (Typed (Value' lab)) (Value' lab)
    {- ^ * Local if-then-else; the first argument is boolean, if
           true pick the 2nd argument, otherwise evaluate to the 3rd.
         * Middle of basic block.
         * Returns either the 2nd or the 3rd argument. -}

  | ExtractValue (Typed (Value' lab)) [Int32]
    {- ^ * Get the value of a member of an aggregate value:
           the first argument is an aggregate value (not a pointer!),
           the second is a path of indexes, similar to the one in 'GEP'.
         * Middle of basic block.
         * Returns the given member of the aggregate value. -}

  | InsertValue (Typed (Value' lab)) (Typed (Value' lab)) [Int32]
    {- ^ * Set the value for a member of an aggregate value:
           the first argument is the value to insert, the second is the
           aggreagate value to be modified.
         * Middle of basic block.
         * Returns an updated aggregate value. -}

  | ExtractElt (Typed (Value' lab)) (Value' lab)
    {- ^ * Get an element from a vector: the first argument is a vector,
           the second an index.
         * Middle of basic block.
         * Returns the element at the given position. -}

  | InsertElt (Typed (Value' lab)) (Typed (Value' lab)) (Value' lab)
    {- ^ * Modify an element of a vector: the first argument is the vector,
           the second the value to be inserted, the third is the index where
           to insert the value.
         * Middle of basic block.
         * Returns an updated vector. -}


  | ShuffleVector (Typed (Value' lab)) (Value' lab) (Typed (Value' lab))


  | Jump lab
    {- ^ * Jump to the given basic block.
         * Ends basic block. -}

  | Br (Typed (Value' lab)) lab lab
    {- ^ * Conditional jump: if the value is true jump to the first basic
           block, otherwise jump to the second.
         * Ends basic block. -}

  | Invoke Type (Value' lab) [Typed (Value' lab)] lab lab

  | Comment String
    -- ^ Comment

  | Unreachable
    -- ^ No defined sematics, we should not get to here.

  | Unwind
  | VaArg (Typed (Value' lab)) Type
  | IndirectBr (Typed (Value' lab)) [lab]

  | Switch (Typed (Value' lab)) lab [(Integer,lab)]
    {- ^ * Multi-way branch: the first value determines the direction
           of the branch, the label is a default direction, if the value
           does not appear in the jump table, the last argument is the
           jump table.
         * Ends basic block. -}

  | LandingPad Type (Maybe (Typed (Value' lab))) Bool [Clause' lab]

  | Resume (Typed (Value' lab))

    deriving (Data, Eq, Functor, Generic, Ord, Show, Typeable)

type Instr = Instr' BlockLabel

data Clause' lab
  = Catch  (Typed (Value' lab))
  | Filter (Typed (Value' lab))
    deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

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

-- | Integer comparison operators.
data ICmpOp = Ieq | Ine | Iugt | Iuge | Iult | Iule | Isgt | Isge | Islt | Isle
    deriving (Data, Eq, Enum, Generic, Ord, Show, Typeable)

-- | Floating-point comparison operators.
data FCmpOp = Ffalse  | Foeq | Fogt | Foge | Folt | Fole | Fone
            | Ford    | Fueq | Fugt | Fuge | Fult | Fule | Fune
            | Funo    | Ftrue
    deriving (Data, Eq, Enum, Generic, Ord, Show, Typeable)


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
  | ValString [Word8]
  | ValConstExpr (ConstExpr' lab)
  | ValUndef
  | ValLabel lab
  | ValZeroInit
  | ValAsm Bool Bool String String
  | ValMd (ValMd' lab)
    deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type Value = Value' BlockLabel

data ValMd' lab
  = ValMdString String
  | ValMdValue (Typed (Value' lab))
  | ValMdRef Int
  | ValMdNode [Maybe (ValMd' lab)]
  | ValMdLoc (DebugLoc' lab)
  | ValMdDebugInfo (DebugInfo' lab)
    deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type ValMd = ValMd' BlockLabel

type KindMd = String
type FnMdAttachments = Map.Map KindMd ValMd
type GlobalMdAttachments = Map.Map KindMd ValMd

data DebugLoc' lab = DebugLoc
  { dlLine  :: Word32
  , dlCol   :: Word32
  , dlScope :: ValMd' lab
  , dlIA    :: Maybe (ValMd' lab)
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

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
    deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

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


-- Constant Expressions --------------------------------------------------------

data ConstExpr' lab
  = ConstGEP Bool (Maybe Word64) (Maybe Type) [Typed (Value' lab)]
  -- ^ Element type introduced in LLVM 3.7
  | ConstConv ConvOp (Typed (Value' lab)) Type
  | ConstSelect (Typed (Value' lab)) (Typed (Value' lab)) (Typed (Value' lab))
  | ConstBlockAddr Symbol lab
  | ConstFCmp FCmpOp (Typed (Value' lab)) (Typed (Value' lab))
  | ConstICmp ICmpOp (Typed (Value' lab)) (Typed (Value' lab))
  | ConstArith ArithOp (Typed (Value' lab)) (Value' lab)
  | ConstBit BitOp (Typed (Value' lab)) (Value' lab)
    deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type ConstExpr = ConstExpr' BlockLabel

-- DWARF Debug Info ------------------------------------------------------------

data DebugInfo' lab
  = DebugInfoBasicType DIBasicType
  | DebugInfoCompileUnit (DICompileUnit' lab)
  | DebugInfoCompositeType (DICompositeType' lab)
  | DebugInfoDerivedType (DIDerivedType' lab)
  | DebugInfoEnumerator String !Int64
  | DebugInfoExpression DIExpression
  | DebugInfoFile DIFile
  | DebugInfoGlobalVariable (DIGlobalVariable' lab)
  | DebugInfoGlobalVariableExpression (DIGlobalVariableExpression' lab)
  | DebugInfoLexicalBlock (DILexicalBlock' lab)
  | DebugInfoLexicalBlockFile (DILexicalBlockFile' lab)
  | DebugInfoLocalVariable (DILocalVariable' lab)
  | DebugInfoSubprogram (DISubprogram' lab)
  | DebugInfoSubrange DISubrange
  | DebugInfoSubroutineType (DISubroutineType' lab)
  | DebugInfoNameSpace (DINameSpace' lab)
  | DebugInfoTemplateTypeParameter (DITemplateTypeParameter' lab)
  | DebugInfoTemplateValueParameter (DITemplateValueParameter' lab)
  | DebugInfoImportedEntity (DIImportedEntity' lab)
    deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DebugInfo = DebugInfo' BlockLabel

type DIImportedEntity = DIImportedEntity' BlockLabel
data DIImportedEntity' lab = DIImportedEntity
    { diieTag    :: DwarfTag
    , diieScope  :: Maybe (ValMd' lab)
    , diieEntity :: Maybe (ValMd' lab)
    , diieFile   :: Maybe (ValMd' lab)
    , diieLine   :: Word32
    , diieName   :: Maybe String
    } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DITemplateTypeParameter = DITemplateTypeParameter' BlockLabel
data DITemplateTypeParameter' lab = DITemplateTypeParameter
    { dittpName :: String
    , dittpType :: ValMd' lab
    } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DITemplateValueParameter = DITemplateValueParameter' BlockLabel
data DITemplateValueParameter' lab = DITemplateValueParameter
    { ditvpName  :: String
    , ditvpType  :: ValMd' lab
    , ditvpValue :: ValMd' lab
    } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DINameSpace = DINameSpace' BlockLabel
data DINameSpace' lab = DINameSpace
    { dinsName  :: String
    , dinsScope :: ValMd' lab
    , dinsFile  :: ValMd' lab
    , dinsLine  :: Word32
    } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

-- TODO: Turn these into sum types
-- See https://github.com/llvm-mirror/llvm/blob/release_38/include/llvm/Support/Dwarf.def
type DwarfAttrEncoding = Word8
type DwarfLang = Word16
type DwarfTag = Word16
type DwarfVirtuality = Word8
-- See https://github.com/llvm-mirror/llvm/blob/release_38/include/llvm/IR/DebugInfoMetadata.h#L175
type DIFlags = Word32
-- This seems to be defined internally as a small enum, and defined
-- differently across versions. Maybe turn this into a sum type once
-- it stabilizes.
type DIEmissionKind = Word8

data DIBasicType = DIBasicType
  { dibtTag      :: DwarfTag
  , dibtName     :: String
  , dibtSize     :: Word64
  , dibtAlign    :: Word64
  , dibtEncoding :: DwarfAttrEncoding
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

data DICompileUnit' lab = DICompileUnit
  { dicuLanguage           :: DwarfLang
  , dicuFile               :: Maybe (ValMd' lab)
  , dicuProducer           :: Maybe String
  , dicuIsOptimized        :: Bool
  , dicuFlags              :: Maybe String
  , dicuRuntimeVersion     :: Word16
  , dicuSplitDebugFilename :: Maybe FilePath
  , dicuEmissionKind       :: DIEmissionKind
  , dicuEnums              :: Maybe (ValMd' lab)
  , dicuRetainedTypes      :: Maybe (ValMd' lab)
  , dicuSubprograms        :: Maybe (ValMd' lab)
  , dicuGlobals            :: Maybe (ValMd' lab)
  , dicuImports            :: Maybe (ValMd' lab)
  , dicuMacros             :: Maybe (ValMd' lab)
  , dicuDWOId              :: Word64
  , dicuSplitDebugInlining :: Bool
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DICompileUnit = DICompileUnit' BlockLabel

data DICompositeType' lab = DICompositeType
  { dictTag            :: DwarfTag
  , dictName           :: Maybe String
  , dictFile           :: Maybe (ValMd' lab)
  , dictLine           :: Word32
  , dictScope          :: Maybe (ValMd' lab)
  , dictBaseType       :: Maybe (ValMd' lab)
  , dictSize           :: Word64
  , dictAlign          :: Word64
  , dictOffset         :: Word64
  , dictFlags          :: DIFlags
  , dictElements       :: Maybe (ValMd' lab)
  , dictRuntimeLang    :: DwarfLang
  , dictVTableHolder   :: Maybe (ValMd' lab)
  , dictTemplateParams :: Maybe (ValMd' lab)
  , dictIdentifier     :: Maybe String
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DICompositeType = DICompositeType' BlockLabel

data DIDerivedType' lab = DIDerivedType
  { didtTag :: DwarfTag
  , didtName :: Maybe String
  , didtFile :: Maybe (ValMd' lab)
  , didtLine :: Word32
  , didtScope :: Maybe (ValMd' lab)
  , didtBaseType :: Maybe (ValMd' lab)
  , didtSize :: Word64
  , didtAlign :: Word64
  , didtOffset :: Word64
  , didtFlags :: DIFlags
  , didtExtraData :: Maybe (ValMd' lab)
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DIDerivedType = DIDerivedType' BlockLabel

data DIExpression = DIExpression
  { dieElements :: [Word64]
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

data DIFile = DIFile
  { difFilename  :: FilePath
  , difDirectory :: FilePath
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

data DIGlobalVariable' lab = DIGlobalVariable
  { digvScope                :: Maybe (ValMd' lab)
  , digvName                 :: Maybe String
  , digvLinkageName          :: Maybe String
  , digvFile                 :: Maybe (ValMd' lab)
  , digvLine                 :: Word32
  , digvType                 :: Maybe (ValMd' lab)
  , digvIsLocal              :: Bool
  , digvIsDefinition         :: Bool
  , digvVariable             :: Maybe (ValMd' lab)
  , digvDeclaration          :: Maybe (ValMd' lab)
  , digvAlignment            :: Maybe Word32
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DIGlobalVariable = DIGlobalVariable' BlockLabel

data DIGlobalVariableExpression' lab = DIGlobalVariableExpression
  { digveVariable   :: Maybe (ValMd' lab)
  , digveExpression :: Maybe (ValMd' lab)
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DIGlobalVariableExpression = DIGlobalVariableExpression' BlockLabel

data DILexicalBlock' lab = DILexicalBlock
  { dilbScope  :: Maybe (ValMd' lab)
  , dilbFile   :: Maybe (ValMd' lab)
  , dilbLine   :: Word32
  , dilbColumn :: Word16
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DILexicalBlock = DILexicalBlock' BlockLabel

data DILexicalBlockFile' lab = DILexicalBlockFile
  { dilbfScope         :: ValMd' lab
  , dilbfFile          :: Maybe (ValMd' lab)
  , dilbfDiscriminator :: Word32
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DILexicalBlockFile = DILexicalBlockFile' BlockLabel

data DILocalVariable' lab = DILocalVariable
  { dilvScope :: Maybe (ValMd' lab)
  , dilvName :: Maybe String
  , dilvFile :: Maybe (ValMd' lab)
  , dilvLine :: Word32
  , dilvType :: Maybe (ValMd' lab)
  , dilvArg :: Word16
  , dilvFlags :: DIFlags
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DILocalVariable = DILocalVariable' BlockLabel

data DISubprogram' lab = DISubprogram
  { dispScope          :: Maybe (ValMd' lab)
  , dispName           :: Maybe String
  , dispLinkageName    :: Maybe String
  , dispFile           :: Maybe (ValMd' lab)
  , dispLine           :: Word32
  , dispType           :: Maybe (ValMd' lab)
  , dispIsLocal        :: Bool
  , dispIsDefinition   :: Bool
  , dispScopeLine      :: Word32
  , dispContainingType :: Maybe (ValMd' lab)
  , dispVirtuality     :: DwarfVirtuality
  , dispVirtualIndex   :: Word32
  , dispThisAdjustment :: Int64
  , dispFlags          :: DIFlags
  , dispIsOptimized    :: Bool
  , dispUnit           :: Maybe (ValMd' lab)
  , dispTemplateParams :: Maybe (ValMd' lab)
  , dispDeclaration    :: Maybe (ValMd' lab)
  , dispVariables      :: Maybe (ValMd' lab)
  , dispThrownTypes    :: Maybe (ValMd' lab)
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DISubprogram = DISubprogram' BlockLabel

data DISubrange = DISubrange
  { disrCount      :: Int64
  , disrLowerBound :: Int64
  } deriving (Data, Eq, Generic, Ord, Show, Typeable)

data DISubroutineType' lab = DISubroutineType
  { distFlags     :: DIFlags
  , distTypeArray :: Maybe (ValMd' lab)
  } deriving (Data, Eq, Functor, Generic, Generic1, Ord, Show, Typeable)

type DISubroutineType = DISubroutineType' BlockLabel

-- Aggregate Utilities ---------------------------------------------------------

data IndexResult
  = Invalid                             -- ^ An invalid use of GEP
  | HasType Type                        -- ^ A resolved type
  | Resolve Ident (Type -> IndexResult) -- ^ Continue, after resolving an alias
  deriving (Generic, Typeable)

isInvalid :: IndexResult -> Bool
isInvalid ir = case ir of
  Invalid -> True
  _       -> False

-- | Resolves the type of a GEP instruction. Type aliases are resolved
-- using the given function. An invalid use of GEP or one relying
-- on unknown type aliases will return 'Nothing'
resolveGepFull ::
  (Ident -> Maybe Type) {- ^ Type alias resolution -} ->
  Type                  {- ^ Pointer type          -} ->
  [Typed (Value' lab)]  {- ^ Path                  -} ->
  Maybe Type            {- ^ Type of result        -}
resolveGepFull env t ixs = go (resolveGep t ixs)
  where
  go Invalid                = Nothing
  go (HasType result)       = Just result
  go (Resolve ident resume) = go . resume =<< env ident


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
isGepIndex tv =
  isPrimTypeOf isInteger (typedType tv) ||
  isVectorOf (isPrimTypeOf isInteger) (typedType tv)

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
