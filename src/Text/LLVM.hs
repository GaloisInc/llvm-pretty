{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Text.LLVM (
    -- * LLVM Monad
    LLVM
  , LLVMT()
  , runLLVM
  , runLLVMT
  , emitTypeDecl
  , emitGlobal
  , emitDeclare
  , emitDefine

    -- * Alias Introduction
  , alias

    -- * Function Definition
  , freshSymbol
  , (:>)(..)
  , define, defineFresh, DefineArgs()
  , define'
  , declare
  , global
  , FunAttrs(..), emptyFunAttrs
    -- * Types
  , iT, ptrT, voidT, arrayT
  , (=:), (-:)

    -- * Values
  , IsValue(..)
  , int
  , integer
  , struct
  , array
  , string

    -- * Basic Blocks
  , BB
  , BBT()
  , runBB
  , runBBT
  , bbStmtModifier
  , freshLabel
  , label
  , comment
  , assign

    -- * Terminator Instructions
  , ret
  , retVoid
  , jump
  , br
  , unreachable
  , unwind

    -- * Binary Operations
  , add, fadd
  , sub, fsub
  , mul, fmul
  , udiv, sdiv, fdiv
  , urem, srem, frem

    -- * Bitwise Binary Operations
  , shl
  , lshr, ashr
  , band, bor, bxor

    -- * Conversion Operations
  , trunc
  , zext
  , sext
  , fptrunc
  , fpext
  , fptoui, fptosi
  , uitofp, sitofp
  , ptrtoint, inttoptr
  , bitcast

    -- * Aggregate Operations
  , extractValue
  , insertValue

    -- * Memory Access and Addressing Operations
  , alloca
  , load
  , store
  , getelementptr
  , nullPtr

    -- * Other Operations
  , icmp
  , fcmp
  , phi, PhiArg, from
  , select
  , call, call_
  , invoke
  , switch
  , shuffleVector

    -- * Re-exported
  , module Text.LLVM.AST
  ) where

import Text.LLVM.AST

import Control.Monad.Fix (MonadFix)
import Data.Char (ord)
import Data.Int (Int8,Int16,Int32,Int64)
import Data.Word (Word32, Word64)
import Data.Maybe (maybeToList)
import Data.String (IsString(..))
import MonadLib hiding (jump,Label)
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map


-- Fresh Names -----------------------------------------------------------------

type Names = Map.Map String Int

-- | Avoid generating the provided name.  When the name already exists, return
-- Nothing.
avoid :: String -> Names -> Maybe Names
avoid name ns =
  case Map.lookup name ns of
    Nothing -> Just (Map.insert name 0 ns)
    Just _  -> Nothing

nextName :: String -> Names -> (String,Names)
nextName pfx ns =
  case Map.lookup pfx ns of
    Nothing -> (fmt (0 :: Int),  Map.insert pfx 1 ns)
    Just ix -> (fmt ix, Map.insert pfx (ix+1) ns)
  where
  fmt i = showString pfx (shows i "")


-- LLVM Monad ------------------------------------------------------------------

newtype LLVMT m a = LLVM
  { unLLVM :: WriterT ModuleBuilder (StateT Names m) a
  } deriving (Functor,Applicative,Monad,MonadFix)

instance MonadT LLVMT where
  lift = LLVM . lift . lift

type LLVM = LLVMT Id

-- | This is an internal object used to provide the Monoid/Semigroup building
-- context for the WriterT.  There is no Semigroup instance for Module itself,
-- because combining modules is not a trivial operation and it can fail
-- (e.g. duplicate symbols/definitions); see the 'LLVM.Combine' module for a
-- proper link-like combining function.  However, the functionality here is not
-- really combining two modules, but instead is constructing a single module from
-- discrete operations and thus we can use the ModuleBuilder newtype wrapper to
-- allow Monoid/Semigroup functionality under this LLVM monad.

newtype ModuleBuilder = ModuleBuilder { getModule :: Module }

instance Semigroup ModuleBuilder where
  (ModuleBuilder m1) <> (ModuleBuilder m2) = ModuleBuilder $ Module
    { modSourceName = modSourceName m1 `mplus` modSourceName m2
    , modTriple = modTriple m1 <> modTriple m2
    , modDataLayout = modDataLayout m1 <> modDataLayout m2
    , modTypes = modTypes m1 <> modTypes m2
    , modUnnamedMd = modUnnamedMd m1 <> modUnnamedMd m2
    , modNamedMd = modNamedMd m1 <> modNamedMd m2
    , modGlobals = modGlobals m1 <> modGlobals m2
    , modDeclares = modDeclares m1 <> modDeclares m2
    , modDefines = modDefines m1 <> modDefines m2
    , modInlineAsm = modInlineAsm m1 <> modInlineAsm m2
    , modAliases = modAliases m1 <> modAliases m2
    , modComdat = modComdat m1 <> modComdat m2
    }

instance Monoid ModuleBuilder where
  mempty = ModuleBuilder emptyModule


freshNameLLVM :: (Monad m) => String -> LLVMT m String
freshNameLLVM pfx = LLVM $ do
  ns <- get
  let (n,ns') = nextName pfx ns
  set ns'
  return n

runLLVM :: LLVM a -> (a,Module)
runLLVM  = runId . runLLVMT

runLLVMT :: (Monad m) => LLVMT m a -> m (a, Module)
runLLVMT = fmap (fmap getModule . fst) . runStateT Map.empty . runWriterT . unLLVM

emitTypeDecl :: (Monad m) => TypeDecl -> LLVMT m ()
emitTypeDecl td = LLVM (put $ ModuleBuilder $ emptyModule { modTypes = [td] })

emitGlobal :: (Monad m) => Global -> LLVMT m (Typed Value)
emitGlobal g =
  do LLVM (put $ ModuleBuilder $ emptyModule { modGlobals = [g] })
     return (ptrT (globalType g) -: globalSym g)

emitDefine :: (Monad m) => Define -> LLVMT m (Typed Value)
emitDefine d =
  do LLVM (put $ ModuleBuilder $ emptyModule { modDefines = [d] })
     return (defFunType d -: defName d)

emitDeclare :: (Monad m) => Declare -> LLVMT m (Typed Value)
emitDeclare d =
  do LLVM (put $ ModuleBuilder $ emptyModule { modDeclares = [d] })
     return (decFunType d -: decName d)

alias :: (Monad m) => Ident -> Type -> LLVMT m ()
alias i ty = emitTypeDecl (TypeDecl i ty)

freshSymbol :: (Monad m) => LLVMT m Symbol
freshSymbol  = Symbol `fmap` freshNameLLVM "f"

-- | Emit a declaration.
declare :: (Monad m) => Type -> Symbol -> [Type] -> Bool -> LLVMT m (Typed Value)
declare rty sym tys va = emitDeclare Declare
  { decLinkage    = Nothing
  , decVisibility = Nothing
  , decRetType    = rty
  , decName       = sym
  , decArgs       = tys
  , decVarArgs    = va
  , decAttrs      = []
  , decComdat     = Nothing
  }

-- | Emit a global declaration.
global :: (Monad m) => GlobalAttrs -> Symbol -> Type -> Maybe Value -> LLVMT m (Typed Value)
global attrs sym ty mbVal = emitGlobal Global
  { globalSym      = sym
  , globalType     = ty
  , globalValue    = toValue `fmap` mbVal
  , globalAttrs    = attrs
  , globalAlign    = Nothing
  , globalMetadata = Map.empty
  }

-- | Output a somewhat clunky representation for a string global, that deals
-- well with escaping in the haskell-source string.
string :: (Monad m) => Symbol -> String -> LLVMT m (Typed Value)
string sym str =
  global emptyGlobalAttrs { gaConstant = True } sym (typedType val)
      (Just (typedValue val))
  where
  bytes = [ int (fromIntegral (ord c)) | c <- str ]
  val   = array (iT 8) bytes


-- Function Definition ---------------------------------------------------------

data FunAttrs = FunAttrs
  { funLinkage    :: Maybe Linkage
  , funVisibility :: Maybe Visibility
  , funGC         :: Maybe GC
  } deriving (Show)

emptyFunAttrs :: FunAttrs
emptyFunAttrs  = FunAttrs
  { funLinkage    = Nothing
  , funVisibility = Nothing
  , funGC         = Nothing
  }


-- XXX Do not export
freshArg :: (Monad m) => Type -> LLVMT m (Typed Ident)
freshArg ty = (Typed ty . Ident) `fmap` freshNameLLVM "a"

infixr 0 :>
data a :> b = a :> b
    deriving Show

-- | Types that can be used to define the body of a function.
class DefineArgs a k m | a m -> k where
  defineBody :: [Typed Ident] -> a -> k -> LLVMT m ([Typed Ident], [BasicBlock])

instance (Monad m) => DefineArgs () (BBT m ()) m where
  defineBody tys () body = lift $ runBBT $ do
    body
    return (reverse tys)

instance (DefineArgs as k m, Monad m) => DefineArgs (Type :> as) (Typed Value -> k) m where
  defineBody args (ty :> as) f = do
    arg <- freshArg ty
    defineBody (arg:args) as (f (toValue `fmap` arg))

-- helper instances for DefineArgs

instance (Monad m) => DefineArgs Type (Typed Value -> BBT m ()) m where
  defineBody tys ty body = defineBody tys (ty :> ()) body

instance (Monad m) => DefineArgs (Type,Type) (Typed Value -> Typed Value -> BBT m ()) m where
  defineBody tys (a,b) body = defineBody tys (a :> b :> ()) body

instance (Monad m) => DefineArgs (Type,Type,Type)
                    (Typed Value -> Typed Value -> Typed Value -> BBT m ()) m where
  defineBody tys (a,b,c) body = defineBody tys (a :> b :> c :> ()) body

-- | Define a function.
define :: (DefineArgs sig k m, Monad m) => FunAttrs -> Type -> Symbol -> sig -> k
       -> LLVMT m (Typed Value)
define attrs rty fun sig k = do
  (args,body) <- defineBody [] sig k
  emitDefine Define
    { defLinkage    = funLinkage attrs
    , defVisibility = funVisibility attrs
    , defName       = fun
    , defRetType    = rty
    , defArgs       = args
    , defVarArgs    = False
    , defAttrs      = []
    , defSection    = Nothing
    , defGC         = funGC attrs
    , defBody       = body
    , defMetadata   = Map.empty
    , defComdat     = Nothing
    }

-- | A combination of define and @freshSymbol@.
defineFresh :: (DefineArgs sig k m, Monad m) => FunAttrs -> Type -> sig -> k
            -> LLVMT m (Typed Value)
defineFresh attrs rty args body = do
  sym <- freshSymbol
  define attrs rty sym args body

-- | Function definition when the argument list isn't statically known.  This is
-- useful when generating code.
define' :: (Monad m) => FunAttrs -> Type -> Symbol -> [Type] -> Bool
        -> ([Typed Value] -> BBT m ())
        -> LLVMT m (Typed Value)
define' attrs rty sym sig va k = do
  args <- mapM freshArg sig
  (_, defBody') <- lift $ runBBT (k (map (fmap toValue) args))
  emitDefine Define
    { defLinkage    = funLinkage attrs
    , defVisibility = funVisibility attrs
    , defName       = sym
    , defRetType    = rty
    , defArgs       = args
    , defVarArgs    = va
    , defAttrs      = []
    , defSection    = Nothing
    , defGC         = funGC attrs
    , defBody       = defBody'
    , defMetadata   = Map.empty
    , defComdat     = Nothing
    }

-- Basic Block Monad -----------------------------------------------------------

newtype BBT m a = BB
  { unBB :: ReaderT (Stmt -> Stmt) (WriterT [BasicBlock] (StateT RW m)) a
  } deriving (Functor,Applicative,Monad,MonadFix)

instance MonadT BBT where
  lift = BB . lift . lift . lift

type BB = BBT Id

-- | The 'bbStmtModifier' function can be used to register a function that can
-- modify the subsequent statements generated into this block.
--
-- For example, the following 'BB' monad code segment will emit a couple of LLVM
-- statements:
--
-- > v <- load (iT 8) globalVar Nothing
-- > call fooFunc [v]
-- > jump end
--
-- But these statements will be \"plain\" in the resulting 'BasicBlock'.  If the
-- caller wishes to add debug Metadata for location, they could instead write:
--
-- > bbStmtModifier (extendMetadata ("dbg", ValMdRef i))
-- >  v <- load (iT 8) globalVar Nothing
-- > bbStmtModifier (extendMetadata ("dbg", ValMdRef j))
-- > call fooFunc [v]
-- > jump end
--
-- Where @i@ and @j@ are the metadata index values of the 'DebugLoc' entries
-- describing the source location of the \"load\" and \"call\"+\"jump\" statements,
-- respectively.

bbStmtModifier :: (Monad m) => (Stmt -> Stmt) -> BBT m a -> BBT m a
bbStmtModifier stmtModifier = BB . local stmtModifier . unBB

avoidName :: (Monad m) => String -> BBT m ()
avoidName name = BB $ do
  rw <- get
  case avoid name (rwNames rw) of
    Just ns' -> set rw { rwNames = ns' }
    Nothing  -> error ("avoidName: " ++ name ++ " already registered")

freshNameBB :: (Monad m) => String -> BBT m String
freshNameBB pfx = BB $ do
  rw <- get
  let (n,ns') = nextName pfx (rwNames rw)
  set rw { rwNames = ns' }
  return n

runBB :: BB a -> (a,[BasicBlock])
runBB = runId . runBBT

runBBT :: (Monad m) => BBT m a -> m (a, [BasicBlock])
runBBT m =
  fmap
    (\((a,bbs),_rw) -> (a,bbs))
    (runStateT emptyRW (runWriterT (runReaderT id (unBB body))))
  where
  -- make sure that the last block is terminated
  body = do
    res <- m
    terminateBasicBlock
    return res

data RW = RW
  { rwNames :: Names
  , rwLabel :: Maybe BlockLabel
  , rwStmts :: Seq.Seq Stmt
  } deriving Show

emptyRW :: RW
emptyRW  = RW
  { rwNames = Map.empty
  , rwLabel = Nothing
  , rwStmts = Seq.empty
  }

rwBasicBlock :: RW -> (RW,Maybe BasicBlock)
rwBasicBlock rw
  | Seq.null (rwStmts rw) = (rw,Nothing)
  | otherwise             =
      let rw' = rw { rwLabel = Nothing, rwStmts = Seq.empty }
          bb  = BasicBlock (rwLabel rw) (F.toList (rwStmts rw))
       in (rw',Just bb)

emitStmt :: (Monad m) => Stmt -> BBT m ()
emitStmt stmt = do
  BB $ do
    rw <- get
    smod <- ask
    set $! rw { rwStmts = rwStmts rw Seq.|> smod stmt }
  when (isTerminator (stmtInstr stmt)) terminateBasicBlock

effect :: (Monad m) => Instr -> BBT m ()
effect i = emitStmt (Effect i mempty [])

observe :: (Monad m) => Type -> Instr -> BBT m (Typed Value)
observe ty i = do
  name <- freshNameBB "r"
  let res = Ident name
  emitStmt (Result res i mempty [])
  return (Typed ty (ValIdent res))


-- Basic Blocks ----------------------------------------------------------------

freshLabel :: (Monad m) => BBT m Ident
freshLabel  = Ident `fmap` freshNameBB "L"

-- | Force termination of the current basic block, and start a new one with the
-- given label.  If the previous block had no instructions defined, it will just
-- be thrown away.
label :: (Monad m) => Ident -> BBT m ()
label l = do
  terminateBasicBlock
  BB $ do
    rw <- get
    set $! rw { rwLabel = Just (Named l) }

instance (Monad m) => IsString (BBT m a) where
  fromString l = do
    label (fromString l)
    return (error ("Label ``" ++ l ++ "'' has no value"))

terminateBasicBlock :: (Monad m) => BBT m ()
terminateBasicBlock  = BB $ do
  rw <- get
  let (rw',bb) = rwBasicBlock rw
  put (maybeToList bb)
  set rw'


-- Type Helpers ----------------------------------------------------------------

iT :: Word32 -> Type
iT  = PrimType . Integer

ptrT :: Type -> Type
ptrT  = PtrTo

voidT :: Type
voidT  = PrimType Void

arrayT :: Word64 -> Type -> Type
arrayT  = Array


-- Value Helpers ---------------------------------------------------------------

class IsValue a where
  toValue :: a -> Value

instance IsValue Value where
  toValue = id

instance IsValue a => IsValue (Typed a) where
  toValue = toValue . typedValue

instance IsValue Bool where
  toValue = ValBool

instance IsValue Integer where
  toValue = ValInteger

instance IsValue Int where
  toValue = ValInteger . toInteger

instance IsValue Int8 where
  toValue = ValInteger . toInteger

instance IsValue Int16 where
  toValue = ValInteger . toInteger

instance IsValue Int32 where
  toValue = ValInteger . toInteger

instance IsValue Int64 where
  toValue = ValInteger . toInteger

instance IsValue Float where
  toValue = ValFloat

instance IsValue Double where
  toValue = ValDouble

instance IsValue Ident where
  toValue = ValIdent

instance IsValue Symbol where
  toValue = ValSymbol

(-:) :: IsValue a => Type -> a -> Typed Value
ty -: a = ty =: toValue a

(=:) :: Type -> a -> Typed a
ty =: a = Typed
  { typedType  = ty
  , typedValue = a
  }

int :: Int -> Value
int  = toValue

integer :: Integer -> Value
integer  = toValue

struct :: Bool -> [Typed Value] -> Typed Value
struct packed tvs
  | packed    = PackedStruct (map typedType tvs) =: ValPackedStruct tvs
  | otherwise = Struct (map typedType tvs)       =: ValStruct tvs

array :: Type -> [Value] -> Typed Value
array ty vs = Typed (Array (fromIntegral (length vs)) ty) (ValArray ty vs)


-- Instructions ----------------------------------------------------------------

comment :: (Monad m) => String -> BBT m ()
comment str = effect (Comment str)

-- | Emit an assignment that uses the given identifier to name the result of the
-- BB operation.
--
-- WARNING: this can throw errors.
assign :: (IsValue a, Monad m) => Ident -> BBT m (Typed a) -> BBT m (Typed Value)
assign r@(Ident name) body = do
  avoidName name
  tv <- body
  rw <- BB get
  case Seq.viewr (rwStmts rw) of

    stmts Seq.:> Result _ i d m ->
      do BB (set rw { rwStmts = stmts Seq.|> Result r i d m })
         return (const (ValIdent r) `fmap` tv)

    _ -> error "assign: invalid argument"

-- | Emit the ``ret'' instruction and terminate the current basic block.
ret :: (IsValue a, Monad m) => Typed a -> BBT m ()
ret tv = effect (Ret (toValue `fmap` tv))

-- | Emit ``ret void'' and terminate the current basic block.
retVoid :: (Monad m) => BBT m ()
retVoid  = effect RetVoid

jump :: (Monad m) => Ident -> BBT m ()
jump l = effect (Jump (Named l))

br :: (IsValue a, Monad m) => Typed a -> Ident -> Ident -> BBT m ()
br c t f = effect (Br (toValue `fmap` c) (Named t) (Named f))

unreachable :: (Monad m) => BBT m ()
unreachable  = effect Unreachable

unwind :: (Monad m) => BBT m ()
unwind  = effect Unwind

binop :: (IsValue a, IsValue b, Monad m)
      => (Typed Value -> Value -> Instr) -> Typed a -> b -> BBT m (Typed Value)
binop k l r = observe (typedType l) (k (toValue `fmap` l) (toValue r))

add :: (IsValue a, IsValue b, Monad m) => Typed a -> b -> BBT m (Typed Value)
add  = binop (Arith (Add False False))

fadd :: (IsValue a, IsValue b, Monad m) => Typed a -> b -> BBT m (Typed Value)
fadd  = binop (Arith FAdd)

sub :: (IsValue a, IsValue b, Monad m) => Typed a -> b -> BBT m (Typed Value)
sub  = binop (Arith (Sub False False))

fsub :: (IsValue a, IsValue b, Monad m) => Typed a -> b -> BBT m (Typed Value)
fsub  = binop (Arith FSub)

mul :: (IsValue a, IsValue b, Monad m) => Typed a -> b -> BBT m (Typed Value)
mul  = binop (Arith (Mul False False))

fmul :: (IsValue a, IsValue b, Monad m) => Typed a -> b -> BBT m (Typed Value)
fmul  = binop (Arith FMul)

udiv :: (IsValue a, IsValue b, Monad m) => Typed a -> b -> BBT m (Typed Value)
udiv  = binop (Arith (UDiv False))

sdiv :: (IsValue a, IsValue b, Monad m) => Typed a -> b -> BBT m (Typed Value)
sdiv  = binop (Arith (SDiv False))

fdiv :: (IsValue a, IsValue b, Monad m) => Typed a -> b -> BBT m (Typed Value)
fdiv  = binop (Arith FDiv)

urem :: (IsValue a, IsValue b, Monad m) => Typed a -> b -> BBT m (Typed Value)
urem  = binop (Arith URem)

srem :: (IsValue a, IsValue b, Monad m) => Typed a -> b -> BBT m (Typed Value)
srem  = binop (Arith SRem)

frem :: (IsValue a, IsValue b, Monad m) => Typed a -> b -> BBT m (Typed Value)
frem  = binop (Arith FRem)

shl :: (IsValue a, IsValue b, Monad m) => Typed a -> b -> BBT m (Typed Value)
shl  = binop (Bit (Shl False False))

lshr :: (IsValue a, IsValue b, Monad m) => Typed a -> b -> BBT m (Typed Value)
lshr  = binop (Bit (Lshr False))

ashr :: (IsValue a, IsValue b, Monad m) => Typed a -> b -> BBT m (Typed Value)
ashr  = binop (Bit (Ashr False))

band :: (IsValue a, IsValue b, Monad m) => Typed a -> b -> BBT m (Typed Value)
band  = binop (Bit And)

bor :: (IsValue a, IsValue b, Monad m) => Typed a -> b -> BBT m (Typed Value)
bor  = binop (Bit Or)

bxor :: (IsValue a, IsValue b, Monad m) => Typed a -> b -> BBT m (Typed Value)
bxor  = binop (Bit Xor)

-- | Returns the value stored in the member field of an aggregate value.
extractValue :: (IsValue a, Monad m) => Typed a -> Int32 -> BBT m (Typed Value)
extractValue ta i =
  let etp = case typedType ta of
              Struct fl -> fl !! fromIntegral i
              Array _l etp' -> etp'
              _ -> error "extractValue not given a struct or array."
   in observe etp (ExtractValue (toValue `fmap` ta) [i])

-- | Inserts a value into the member field of an aggregate value, and returns
-- the new value.
insertValue :: (IsValue a, IsValue b, Monad m)
            => Typed a -> Typed b -> Int32 -> BBT m (Typed Value)
insertValue ta tv i =
  observe (typedType ta)
      (InsertValue (toValue `fmap` ta) (toValue `fmap` tv) [i])

shuffleVector :: (IsValue a, IsValue b, IsValue c, Monad m)
              => Typed a -> b -> c -> BBT m (Typed Value)
shuffleVector vec1 vec2 mask =
  case typedType vec1 of
    Vector n _ -> observe (typedType vec1)
                $ ShuffleVector (toValue `fmap` vec1) (toValue vec2)
                $ Typed (Vector n (PrimType (Integer 32))) (toValue mask)
    _          -> error "shuffleVector not given a vector"

alloca :: (Monad m) => Type -> Maybe (Typed Value) -> Maybe Int -> BBT m (Typed Value)
alloca ty mb align = observe (PtrTo ty) (Alloca ty es align)
  where
  es = fmap toValue `fmap` mb

load :: (IsValue a, Monad m) => Type -> Typed a -> Maybe Align -> BBT m (Typed Value)
load ty ptr ma = observe ty (Load ty (toValue `fmap` ptr) Nothing ma)

store :: (IsValue a, IsValue b, Monad m) => a -> Typed b -> Maybe Align -> BBT m ()
store a ptr ma =
  case typedType ptr of
    PtrTo ty -> effect (Store (ty -: a) (toValue `fmap` ptr) Nothing ma)
    _        -> error "store not given a pointer"

nullPtr :: Type -> Typed Value
nullPtr ty = ptrT ty =: ValNull

convop :: (IsValue a, Monad m)
       => (Typed Value -> Type -> Instr) -> Typed a -> Type -> BBT m (Typed Value)
convop k a ty = observe ty (k (toValue `fmap` a) ty)

trunc :: (IsValue a, Monad m) => Typed a -> Type -> BBT m (Typed Value)
trunc  = convop (Conv (Trunc False False))

zext :: (IsValue a, Monad m) => Typed a -> Type -> BBT m (Typed Value)
zext  = convop (Conv (ZExt False))

sext :: (IsValue a, Monad m) => Typed a -> Type -> BBT m (Typed Value)
sext  = convop (Conv SExt)

fptrunc :: (IsValue a, Monad m) => Typed a -> Type -> BBT m (Typed Value)
fptrunc  = convop (Conv FpTrunc)

fpext :: (IsValue a, Monad m) => Typed a -> Type -> BBT m (Typed Value)
fpext  = convop (Conv FpExt)

fptoui :: (IsValue a, Monad m) => Typed a -> Type -> BBT m (Typed Value)
fptoui  = convop (Conv FpToUi)

fptosi :: (IsValue a, Monad m) => Typed a -> Type -> BBT m (Typed Value)
fptosi  = convop (Conv FpToSi)

uitofp :: (IsValue a, Monad m) => Typed a -> Type -> BBT m (Typed Value)
uitofp  = convop (Conv (UiToFp False))

sitofp :: (IsValue a, Monad m) => Typed a -> Type -> BBT m (Typed Value)
sitofp  = convop (Conv SiToFp)

ptrtoint :: (IsValue a, Monad m) => Typed a -> Type -> BBT m (Typed Value)
ptrtoint  = convop (Conv PtrToInt)

inttoptr :: (IsValue a, Monad m) => Typed a -> Type -> BBT m (Typed Value)
inttoptr  = convop (Conv IntToPtr)

bitcast :: (IsValue a, Monad m) => Typed a -> Type -> BBT m (Typed Value)
bitcast  = convop (Conv BitCast)

icmp :: (IsValue a, IsValue b, Monad m) => ICmpOp -> Typed a -> b -> BBT m (Typed Value)
icmp op l r = observe (iT 1) (ICmp False op (toValue `fmap` l) (toValue r))

fcmp :: (IsValue a, IsValue b, Monad m) => FCmpOp -> Typed a -> b -> BBT m (Typed Value)
fcmp op l r = observe (iT 1) (FCmp op (toValue `fmap` l) (toValue r))

data PhiArg = PhiArg Value BlockLabel

from :: IsValue a => a -> BlockLabel -> PhiArg
from a = PhiArg (toValue a)

phi :: (Monad m) => Type -> [PhiArg] -> BBT m (Typed Value)
phi ty vs = observe ty (Phi ty [ (v,l) | PhiArg v l <- vs ])

select :: (IsValue a, IsValue b, IsValue c, Monad m)
       => Typed a -> Typed b -> Typed c -> BBT m (Typed Value)
select c t f = observe (typedType t)
             $ Select (toValue `fmap` c) (toValue `fmap` t) (toValue f)

getelementptr :: (IsValue a, Monad m)
              => Type -> Typed a -> [Typed Value] -> BBT m (Typed Value)
getelementptr ty ptr ixs = observe ty (GEP [] ty (toValue `fmap` ptr) ixs)

-- | Emit a call instruction, and generate a new variable for its result.
call :: (IsValue a, Monad m) => Typed a -> [Typed Value] -> BBT m (Typed Value)
call sym vs = case typedType sym of
  PtrTo ty@(FunTy rty _ _) -> observe rty (Call False ty (toValue sym) vs)
  _                        -> error "invalid function type given to call"

-- | Emit a call instruction, but don't generate a new variable for its result.
call_ :: (IsValue a, Monad m) => Typed a -> [Typed Value] -> BBT m ()
call_ sym vs = effect (Call False (typedType sym) (toValue sym) vs)

-- | Emit an invoke instruction, and generate a new variable for its result.
invoke :: (IsValue a, Monad m) =>
          Type -> a -> [Typed Value] -> Ident -> Ident -> BBT m (Typed Value)
invoke rty sym vs to uw = observe rty
                        $ Invoke rty (toValue sym) vs (Named to) (Named uw)

-- | Emit a call instruction, but don't generate a new variable for its result.
switch :: (IsValue a, Monad m) => Typed a -> Ident -> [(Integer, Ident)] -> BBT m ()
switch idx def dests = effect (Switch (toValue `fmap` idx) (Named def)
                                      (map (\(n, l) -> (n, Named l)) dests))
