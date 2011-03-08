{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}

module Text.LLVM (
    -- * LLVM Monad
    LLVM()
  , runLLVM

    -- * Basic-block Monad
  , BB()

    -- * Values
  , Value
  , fromLit
  , HasType(..)
  , HasValues(..)
  , HasLiterals(..)

    -- * Functions
  , Fun, IsFun(), HasFun(), Res
  , simpleFun, funWithAttrs
  , FunAttrs(..), emptyFunAttrs
  , funAddr, FunPtr
  , define, Define()
  , declare, Declare()
  , call, tailCall, Call()
  , call_, tailCall_, Call_()

    -- * Labels
  , Label(..)
  , defineLabel
  , freshLabel

    -- * Pointers
  , PtrTo
  , nullPtr

    -- * Structs
  , (:>)()
  , Struct()
  , Packed()

    -- * Arrays
  , Array()

    -- * Instructions
  , comment
  , ret, retVoid
  , add, fadd
  , sub, fsub
  , mul, fmul
  , udiv, sdiv, fdiv
  , urem, srem, frem
  , icmp, ICmpOp(..)
  , fcmp, FCmpOp(..)
  , br, jump
  , unreachable
  , unwind
  , alloca
  , load
  , store
  , phi
  , bitcast
  , getelementptr
  , ptrtoint
  , inttoptr
  ) where

import Text.LLVM.AST (ICmpOp(..),FCmpOp(..))
import qualified Text.LLVM.AST as AST

import Control.Applicative (Applicative(..))
import Control.Monad.Fix (MonadFix(..))
import Data.Int (Int8,Int16,Int32,Int64)
import Data.List (intersperse)
import Data.Monoid (Monoid(..))
import MonadLib hiding (Label,jump)
import Numeric (showHex)
import Text.PrettyPrint.HughesPJ


-- Fresh Names -----------------------------------------------------------------

class FreshName m where
  freshName :: String -> m String


-- Top-level LLVM Monad --------------------------------------------------------

newtype LLVM a = LLVM
  { unLLVM :: StateT Int (WriterT AST.Module Id) a
  } deriving (Functor,Applicative,Monad,MonadFix)

runLLVM :: LLVM a -> (a,AST.Module)
runLLVM (LLVM m) = (a,w)
  where
  ((a,_),w) = runId (runWriterT (runStateT 0 m))

instance WriterM LLVM AST.Module where
  put = LLVM . put

instance RunWriterM LLVM AST.Module where
  collect m = LLVM (collect (unLLVM m))

instance FreshName LLVM where
  freshName pfx = LLVM $ do
    i <- get
    set $! i + 1
    return (pfx ++ show i)

emitTypeDecl :: AST.TypeDecl -> LLVM ()
emitTypeDecl d = put AST.emptyModule
  { AST.modTypes = [d]
  }

emitGlobal :: AST.Global -> LLVM ()
emitGlobal d = put AST.emptyModule
  { AST.modGlobals = [d]
  }

emitDeclare :: AST.Declare -> LLVM ()
emitDeclare d = put AST.emptyModule
  { AST.modDeclares = [d]
  }

emitDefine :: AST.Define -> LLVM ()
emitDefine d = put AST.emptyModule
  { AST.modDefines = [d]
  }


-- Basic Block Monad -----------------------------------------------------------

newtype BB r a = BB
  { unBB :: StateT Int (WriterT [AST.Stmt] Id) a
  } deriving (Functor,Applicative,Monad,MonadFix)

runBB :: BB r a -> (a,[AST.Stmt])
runBB (BB m) = (a,w)
  where
  ((a,_),w) = runId (runWriterT (runStateT 0 m))

instance WriterM (BB r) [AST.Stmt] where
  put = BB . put

instance RunWriterM (BB r) [AST.Stmt] where
  collect m = BB (collect (unBB m))

instance FreshName (BB r) where
  freshName pfx = BB $ do
    i <- get
    set $! i + 1
    return (pfx ++ show i)

emitStmt :: AST.Stmt -> BB r ()
emitStmt s = put [s]

effect :: AST.Instr -> BB r ()
effect  = emitStmt . AST.Effect

observe :: HasType a => AST.Instr -> BB r (Value a)
observe instr = do
  name <- freshName "res"
  let res = AST.Ident name
  emitStmt (AST.Result res instr)
  return (Value (AST.ValIdent res))


-- Representable Types ---------------------------------------------------------

class HasType t where
  getType :: t -> AST.Type

instance HasType () where
  getType _ = AST.PrimType AST.Void

instance HasType Bool where
  getType _ = AST.PrimType (AST.Integer 1)

instance HasType Int8 where
  getType _ = AST.PrimType (AST.Integer 8)

instance HasType Int16 where
  getType _ = AST.PrimType (AST.Integer 16)

instance HasType Int32 where
  getType _ = AST.PrimType (AST.Integer 32)

instance HasType Int64 where
  getType _ = AST.PrimType (AST.Integer 64)

instance HasType Float where
  getType _ = AST.PrimType (AST.FloatType AST.Float)

instance HasType Double where
  getType _ = AST.PrimType (AST.FloatType AST.Double)


-- Values ----------------------------------------------------------------------

newtype Value a = Value { unValue :: AST.Value }

valueType :: Value a -> a
valueType  = error "valueType"

typedValue :: HasValues a => Value a -> AST.Typed AST.Value
typedValue v = AST.Typed (getType (valueType v)) (unValue v)

class HasType t => HasValues t

instance HasValues Bool
instance HasValues Int8
instance HasValues Int16
instance HasValues Int32
instance HasValues Int64
instance HasValues Float
instance HasValues Double


-- Literals --------------------------------------------------------------------

-- | Construct a tagged value, from a literal.
fromLit :: HasLiterals t => t -> Value t
fromLit  = Value . getValue

class HasValues t => HasLiterals t where
  getValue :: t -> AST.Value

instance HasLiterals Bool where
  getValue False = AST.ValInteger 0
  getValue True  = AST.ValInteger 1

instance HasLiterals Int8 where
  getValue = AST.ValInteger . fromIntegral

instance HasLiterals Int16 where
  getValue = AST.ValInteger . fromIntegral

instance HasLiterals Int32 where
  getValue = AST.ValInteger . fromIntegral

instance HasLiterals Int64 where
  getValue = AST.ValInteger . fromIntegral

instance HasLiterals Float where
  getValue = AST.ValFloat

instance HasLiterals Double where
  getValue = AST.ValDouble


-- Functions -------------------------------------------------------------------

-- | A function symbol.
data Fun f = Fun
  { funSymbol :: AST.Symbol
  , funAttrs  :: FunAttrs
  }

-- | Create a function symbol with the given name, and no additional attributes.
simpleFun :: IsFun f => String -> Fun f
simpleFun sym = funWithAttrs sym emptyFunAttrs

-- | Create a function symbol with the given name and attributes.
funWithAttrs :: IsFun f => String -> FunAttrs -> Fun f
funWithAttrs sym attrs = Fun
  { funSymbol = AST.Symbol sym
  , funAttrs  = attrs
  }

type FunPtr f = PtrTo (Fun f)

funPtrTail :: Value (FunPtr (a -> b)) -> Value (FunPtr b)
funPtrTail (Value v) = Value v

funPtrType :: FunPtr a -> a
funPtrType  = error "funPtrType"

-- | Take the address of a function.
funAddr :: IsFun f => Fun f -> Value (FunPtr f)
funAddr f = Value (AST.ValSymbol (funSymbol f))

instance IsFun f => HasType (Fun f) where
  getType = uncurry AST.FunTy . getFunType

instance IsFun f => HasValues (Fun f)


funType :: Fun f -> f
funType  = error "funType"

funHead :: Fun (a -> b) -> Fun a
funHead f = Fun
  { funSymbol = funSymbol f
  , funAttrs  = funAttrs f
  }

funTail :: Fun (a -> b) -> Fun b
funTail f = Fun
  { funSymbol = funSymbol f
  , funAttrs  = funAttrs f
  }

class IsFun f where
  getFunType :: Fun f -> (AST.Type,[AST.Type])

instance HasType a => IsFun (Res a) where
  getFunType f = (getType (resType (funType f)), [])

instance (HasValues a, IsFun f) => IsFun (a -> f) where
  getFunType f = (rty, getType (funType (funHead f)):args)
    where
    (rty,args) = getFunType (funTail f)

-- | Function attributes.
data FunAttrs = FunAttrs
  { funLinkage :: Maybe AST.Linkage
  , funGC      :: Maybe AST.GC
  }

-- | No function attributes.
emptyFunAttrs :: FunAttrs
emptyFunAttrs  = FunAttrs
  { funLinkage = Nothing
  , funGC      = Nothing
  }

data Res a = Res

resType :: Res a -> a
resType  = error "resType"


-- | Define a function identified by the symbol provided.
define :: Define f g => Fun f -> g -> LLVM ()
define  = defineBody []

class Define f g | f -> g, g -> f where
  defineBody :: [AST.Typed AST.Ident] -> Fun f -> g -> LLVM ()

instance HasType r => Define (Res r) (BB r ()) where
  defineBody is f body = do
    let retTy     = getType (resType (funType f))
    let attrs     = funAttrs f
    let (_,stmts) = runBB body
    emitDefine AST.Define
      { AST.defLinkage = funLinkage attrs
      , AST.defRetType = retTy
      , AST.defName    = funSymbol f
      , AST.defArgs    = reverse is
      , AST.defGC      = funGC attrs
      , AST.defBody    = stmts
      }

instance (HasValues a, Define f g) => Define (a -> f) (Value a -> g) where
  defineBody is f g = do
    name <- freshName "arg"
    let i   = AST.Ident name
        arg = Value (AST.ValIdent i)
        ty  = AST.Typed (getType (funType (funHead f))) i
    defineBody (ty:is) (funTail f) (g arg)


-- | Declare an external symbol with the tagged type of the given function
-- symbol.
declare :: Declare f => Fun f -> LLVM ()
declare  = declareBody []

class Declare f where
  declareBody :: [AST.Type] -> Fun f -> LLVM ()

instance HasType r => Declare (Res r) where
  declareBody tys f = do
    let retTy = getType (resType (funType f))
    emitDeclare AST.Declare
      { AST.decRetType = retTy
      , AST.decName    = funSymbol f
      , AST.decArgs    = reverse tys
      }

instance (HasValues a, Declare f) => Declare (a -> f) where
  declareBody tys f = do
    let ty = getType (funType (funHead f))
    declareBody (ty:tys) (funTail f)


-- | Types that represent a function.
class HasFun f g | f -> g where
  getFun :: f -> Value (FunPtr g)

instance IsFun f => HasFun (Value (FunPtr f)) f where
  getFun = id

instance IsFun f => HasFun (Fun f) f where
  getFun = funAddr


-- | Call a function, naming its result.
call :: (HasFun f g, Call g k) => f -> k
call  = callBody False [] . getFun

-- | Call a function, naming its result, and signaling the tail call
-- optimization.
tailCall :: (HasFun f g, Call g k) => f -> k
tailCall  = callBody True [] . getFun

-- | Invocations of the call instruction, that name its result.
class Call f k | f -> k, k -> f where
  callBody :: Bool -> [AST.Typed AST.Value] -> Value (FunPtr f) -> k

instance HasValues a => Call (Res a) (BB r (Value a)) where
  callBody tc as f = do
    name <- freshName "res"
    let i     = AST.Ident name
    let res   = Value (AST.ValIdent i)
    let resTy = getType (resType (funPtrType (valueType f)))
    let sym   = unValue f
    emitStmt (AST.Result i (AST.call tc resTy sym (reverse as)))
    return res

instance (HasValues a, Call f k) => Call (a -> f) (Value a -> k) where
  callBody tc as f v = callBody tc (typedValue v:as) (funPtrTail f)


-- | Call a function, ignoring its return value.
call_ :: (HasFun f g, Call_ g k) => f -> k
call_  = callBody_ False [] . getFun

-- | Call a function, ignoring its return value, and signaling the tail call
-- optimization.
tailCall_ :: (HasFun f g, Call_ g k) => f -> k
tailCall_  = callBody_ True [] . getFun

-- | Invocations of the call instruction, that ignore its result.
class Call_ f k | f -> k, k -> f where
  callBody_ :: Bool -> [AST.Typed AST.Value] -> Value (FunPtr f) -> k

instance HasType a => Call_ (Res a) (BB r ()) where
  callBody_ tc as f = do
    name <- freshName "res"
    let resTy = getType (resType (funPtrType (valueType f)))
    let sym   = unValue f
    effect (AST.call tc resTy sym (reverse as))

instance (HasValues a, Call_ f k) => Call_ (a -> f) (Value a -> k) where
  callBody_ tc as f v = callBody_ tc (typedValue v:as) (funPtrTail f)


-- Labels ----------------------------------------------------------------------

data Label = Label
  { labelIdent :: AST.Ident
  }

-- | Define a label.
defineLabel :: Label -> BB r a -> BB r a
defineLabel l body = do
  emitStmt (AST.DefLabel (labelIdent l))
  body

-- | Generate a fresh label.
freshLabel :: BB r Label
freshLabel  = (Label . AST.Ident) `fmap` freshName "L"


-- Pointers --------------------------------------------------------------------

data PtrTo a = PtrTo

-- | The null pointer.
nullPtr :: HasType a => Value (PtrTo a)
nullPtr  = Value AST.ValNull

instance HasType a => HasType (PtrTo a) where
  getType ptr = AST.PtrTo (getType (ptrType ptr))

instance HasType a => HasValues (PtrTo a)

ptrType :: PtrTo a -> a
ptrType  = error "ptrType"


-- Structures ------------------------------------------------------------------

-- | A type-level list.
data (:>) a b = C
infixr 1 :>

listHead :: a :> b -> a
listHead  = error "listHead"

listTail :: a :> b -> b
listTail  = error "listTail"

-- | A structure.
data Struct s = S

structType :: Struct s -> s
structType  = error "structType"

-- | A packed structure.
data Packed s = P

packedType :: Packed s -> s
packedType  = error "structType"


class IsStruct s where
  structTypes :: s -> [AST.Type]

instance IsStruct () where
  structTypes _ = []

instance (HasValues a, IsStruct s) => IsStruct (a :> s) where
  structTypes s = getType (listHead s) : structTypes (listTail s)


instance IsStruct s => HasType (Struct s) where
  getType = AST.Struct . structTypes . structType

instance IsStruct s => HasValues (Struct s)

instance IsStruct s => HasType (Packed s) where
  getType = AST.PackedStruct . structTypes . packedType

instance IsStruct s => HasValues (Packed s)


-- Arrays ----------------------------------------------------------------------

-- | All arrays are zero-length, until there is better support for type-level
-- natural numbers in GHC.
data Array a = A

arrayType :: Array a -> a
arrayType  = error "arrayType"

instance HasValues a => HasType (Array a) where
  getType a = AST.Array 0 (getType (arrayType a))

instance HasValues a => HasValues (Array a)


-- Instructions ----------------------------------------------------------------

comment :: String -> BB r ()
comment  = effect . AST.comment

ret :: HasValues r => Value r -> BB r ()
ret v = effect (AST.ret (typedValue v))

retVoid :: BB () ()
retVoid  = effect AST.retVoid

-- | Emit the add instruction.
add :: (Integral a, HasValues a) => Value a -> Value a -> BB r (Value a)
add l (Value r) = observe (AST.add (typedValue l) r)

-- | Emit the fadd instruction.
fadd :: (Floating a, HasValues a) => Value a -> Value a -> BB r (Value a)
fadd l (Value r) = observe (AST.fadd (typedValue l) r)

-- | Emit the sub instruction.
sub :: (Integral a, HasValues a) => Value a -> Value a -> BB r (Value a)
sub l (Value r) = observe (AST.sub (typedValue l) r)

-- | Emit the fsub instruction.
fsub :: (Floating a, HasValues a) => Value a -> Value a -> BB r (Value a)
fsub l (Value r) = observe (AST.fsub (typedValue l) r)

-- | Emit the mul instruction.
mul :: (Integral a, HasValues a) => Value a -> Value a -> BB r (Value a)
mul l (Value r) = observe (AST.mul (typedValue l) r)

-- | Emit the fmul instruction.
fmul :: (Integral a, HasValues a) => Value a -> Value a -> BB r (Value a)
fmul l (Value r) = observe (AST.fmul (typedValue l) r)

-- | Emit the udiv instruction.
udiv :: (Integral a, HasValues a) => Value a -> Value a -> BB r (Value a)
udiv l (Value r) = observe (AST.udiv (typedValue l) r)

-- | Emit the sdiv instruction.
sdiv :: (Integral a, HasValues a) => Value a -> Value a -> BB r (Value a)
sdiv l (Value r) = observe (AST.sdiv (typedValue l) r)

-- | Emit the fdiv instruction.
fdiv :: (Integral a, HasValues a) => Value a -> Value a -> BB r (Value a)
fdiv l (Value r) = observe (AST.fdiv (typedValue l) r)

-- | Emit the urem instruction.
urem :: (Integral a, HasValues a) => Value a -> Value a -> BB r (Value a)
urem l (Value r) = observe (AST.urem (typedValue l) r)

-- | Emit the srem instruction.
srem :: (Integral a, HasValues a) => Value a -> Value a -> BB r (Value a)
srem l (Value r) = observe (AST.srem (typedValue l) r)

-- | Emit the frem instruction.
frem :: (Integral a, HasValues a) => Value a -> Value a -> BB r (Value a)
frem l (Value r) = observe (AST.frem (typedValue l) r)

icmp :: HasValues a => ICmpOp -> Value a -> Value a -> BB r (Value Bool)
icmp op l (Value r) = observe (AST.icmp op (typedValue l) r)

fcmp :: (Floating a, HasValues a)
     => FCmpOp -> Value a -> Value a -> BB r (Value Bool)
fcmp op l (Value r) = observe (AST.fcmp op (typedValue l) r)

-- | The branch instruction.
br :: Value Bool -> Label -> Label -> BB r ()
br b t f = effect (AST.condBr (unValue b) (labelIdent t) (labelIdent f))

-- | Unconditional branch.
jump :: Label -> BB r ()
jump l = effect (AST.br (labelIdent l))

-- | Conditional branch.

-- | The unreachable instruction.
unreachable :: BB r ()
unreachable  = effect AST.unreachable

-- | Unwind the stack.
unwind :: BB r ()
unwind  = effect AST.unwind

allocaBody :: HasValues a
           => Maybe (Value Int32) -> Maybe Int -> BB r (Value (PtrTo a))
allocaBody mbLen mbAlign = mfix $ \ a -> do
  let ty    = getType (ptrType (valueType a))
  let len   = typedValue `fmap` mbLen
  observe (AST.alloca ty len mbAlign)

-- | Allocate some memory on the stack.
class Alloca f where
  alloca :: f

instance HasValues a => Alloca (BB r (Value (PtrTo a))) where
  alloca = allocaBody Nothing Nothing

instance HasValues a => Alloca (Value Int32 -> BB r (Value (PtrTo a))) where
  alloca len = allocaBody (Just len) Nothing

instance HasValues a => Alloca (Int -> BB r (Value (PtrTo a))) where
  alloca align = allocaBody Nothing (Just align)

instance HasValues a =>
    Alloca (Value Int32 -> Int -> BB r (Value (PtrTo a))) where
  alloca len align = allocaBody (Just len) (Just align)

-- | Load the value stored in a pointer.
load :: HasValues a => Value (PtrTo a) -> BB r (Value a)
load v = observe (AST.load (typedValue v))

-- | Store a value in a pointer.
store :: HasValues a => Value a -> Value (PtrTo a) -> BB r ()
store a p = effect (AST.store (typedValue a) (typedValue p))

-- | Merge one or more values into the same name.
phi :: HasValues a => Value a -> Label -> [(Value a, Label)] -> BB r (Value a)
phi v l vls = observe (AST.phi (getType (valueType v)) args)
  where
  args = step (v,l) : map step vls
  step (a,b) = (unValue a, labelIdent b)

-- | Cast values from one aggregate, valued type to another.
bitcast :: (HasValues a, HasValues b) => Value a -> BB r (Value b)
bitcast va =
  mfix (\vb -> observe (AST.bitcast (typedValue va) (getType (valueType vb))))

getelementptr :: (HasValues a, HasValues b)
              => Value (PtrTo a) -> Value Int32 -> [Value Int32]
              -> BB r (Value (PtrTo b))
getelementptr v ix ixs = observe (AST.getelementptr (typedValue v) args)
  where
  args = typedValue ix : map typedValue ixs

ptrtoint :: (HasType a, HasValues b) => Value (PtrTo a) -> BB r (Value b)
ptrtoint v =
  mfix (\i -> observe (AST.ptrtoint (typedValue v) (getType (valueType i))))

inttoptr :: (HasValues a, HasType b) => Value a -> BB r (Value (PtrTo b))
inttoptr v =
  mfix (\i -> observe (AST.inttoptr (typedValue v) (getType (valueType i))))


-- Tests -----------------------------------------------------------------------

test = snd $ runLLVM $ do
  let fact :: Fun (Int32 -> Res Int32)
      fact  = simpleFun "fact"
  define fact $ \a -> do
    recurse <- freshLabel
    exit    <- freshLabel

    b <- icmp Ieq a (fromLit 0)
    br b exit recurse

    defineLabel exit (ret (fromLit 1))

    defineLabel recurse $ do
      val <- call fact =<< sub a (fromLit 1)
      ret =<< mul a val

    return ()

  let main :: Fun (Res Int32)
      main  = simpleFun "main"
  define main $ do
    call_ (funAddr fact) (fromLit 10)
    ret (fromLit 0)
