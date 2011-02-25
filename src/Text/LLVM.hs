{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.LLVM (
    -- * LLVM Monad
    LLVM()
  , runLLVM

    -- * Basic-block Monad
  , BB()

    -- * Functions
  , Fun, IsFun()
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

    -- * Instructions
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
  }

instance IsFun f => HasType (Fun f) where
  getType = uncurry AST.FunTy . getFunType

instance IsFun f => HasValues (Fun f)

class IsFun f where
  getFunType :: Fun f -> (AST.Type,[AST.Type])

instance HasType a => IsFun (Res a) where
  getFunType f = (getType (resType (funType f)), [])

instance (HasValues a, IsFun f) => IsFun (a -> f) where
  getFunType f = (rty, getType (funType (funHead f)):args)
    where
    (rty,args) = getFunType (funTail f)

funType :: Fun f -> f
funType  = error "funType"

funHead :: Fun (a -> b) -> Fun a
funHead f = Fun
  { funSymbol = funSymbol f
  }

funTail :: Fun (a -> b) -> Fun b
funTail f = Fun
  { funSymbol = funSymbol f
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
    let (_,stmts) = runBB body
    emitDefine AST.Define
      { AST.defRetType = retTy
      , AST.defName    = funSymbol f
      , AST.defArgs    = reverse is
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


-- | Call a function, naming its result.
call :: Call f k => Fun f -> k
call  = callBody False []

-- | Call a function, naming its result, and signaling the tail call
-- optimization.
tailCall :: Call f k => Fun f -> k
tailCall  = callBody True []

class Call f k | f -> k, k -> f where
  callBody :: Bool -> [AST.Typed AST.Value] -> Fun f -> k

instance HasValues a => Call (Res a) (BB r (Value a)) where
  callBody tc as f = do
    name <- freshName "res"
    let i     = AST.Ident name
    let res   = Value (AST.ValIdent i)
    let resTy = getType (resType (funType f))
    emitStmt (AST.Result i (AST.call tc resTy (funSymbol f) (reverse as)))
    return res

instance (HasValues a, Call f k) => Call (a -> f) (Value a -> k) where
  callBody tc as f v = callBody tc (typedValue v:as) (funTail f)


-- | Call a function, ignoring its return value.
call_ :: Call_ f k => Fun f -> k
call_  = callBody_ False []

-- | Call a function, ignoring its return value, and signaling the tail call
-- optimization.
tailCall_ :: Call_ f k => Fun f -> k
tailCall_  = callBody_ True []

class Call_ f k | f -> k, k -> f where
  callBody_ :: Bool -> [AST.Typed AST.Value] -> Fun f -> k

instance HasType a => Call_ (Res a) (BB r ()) where
  callBody_ tc as f = do
    name <- freshName "res"
    let resTy = getType (resType (funType f))
    effect (AST.call tc resTy (funSymbol f) (reverse as))

instance (HasValues a, Call_ f k) => Call_ (a -> f) (Value a -> k) where
  callBody_ tc as f v = callBody_ tc (typedValue v:as) (funTail f)


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


-- Instructions ----------------------------------------------------------------

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


-- Tests -----------------------------------------------------------------------

test = snd $ runLLVM $ do
  let fact :: Fun (Int32 -> Res Int32)
      fact  = Fun (AST.Symbol "fact")
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
