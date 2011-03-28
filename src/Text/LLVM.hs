{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.LLVM (
    -- * LLVM Monad
    LLVM()
  , runLLVM

    -- * Function Definition
  , freshSymbol
  , define

    -- * Types
  , iT, voidT
  , IsValue(..), int
  , (=:), (-:)

    -- * Basic Blocks
  , BB()
  , freshLabel
  , label
  , comment

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

    -- * Memory Access and Addressing Operations
  , alloca
  , load
  , store

    -- * Other Operations
  , icmp, ICmpOp(..)
  , fcmp, FCmpOp(..)
  , phi
  , select
  , call, call_
  ) where

import Text.LLVM.AST
    (Module(..),Ident(..),Stmt(..),BasicBlock(..),Typed(..),Value(..),Type(..)
    ,PrimType(..),FloatType(..),Instr,Define(..),emptyModule,Symbol(..)
    ,ICmpOp(..),FCmpOp(..))
import qualified Text.LLVM.AST as AST

import Control.Monad.Fix (MonadFix)
import Data.Int (Int32)
import Data.Maybe (maybeToList)
import MonadLib hiding (jump,Label)


-- Fresh Names -----------------------------------------------------------------

class FreshName m where
  freshName :: String -> m String


-- LLVM Monad ------------------------------------------------------------------

newtype LLVM a = LLVM
  { unLLVM :: WriterT Module (StateT Int Id) a
  } deriving (Functor,Monad,MonadFix)

instance FreshName LLVM where
  freshName pfx = LLVM $ do
    i <- get
    set $! i + 1
    return (showString pfx (shows i ""))

runLLVM :: LLVM a -> (a,Module)
runLLVM  = fst . runId . runStateT 0 . runWriterT . unLLVM

emitDefine :: Define -> LLVM ()
emitDefine d = LLVM (put emptyModule { modDefines = [d] })

freshSymbol :: LLVM Symbol
freshSymbol  = Symbol `fmap` freshName "f"

define :: Type -> Symbol -> [Typed Ident] -> ([Typed Value] -> BB ())
       -> LLVM Value
define rty fun args body = emitDefine def >> return (ValSymbol fun)
  where
  def = Define
    { defLinkage = Nothing
    , defName    = fun
    , defRetType = rty
    , defArgs    = args
    , defGC      = Nothing
    , defBody    = snd (runBB (body (map (fmap toValue) args)))
    }

-- | A combination of define and @freshSymbol@.
defineFresh :: Type -> [Typed Ident] -> ([Typed Value] -> BB ()) -> LLVM Value
defineFresh rty args body = do
  sym <- freshSymbol
  define rty sym args body


-- Basic Block Monad -----------------------------------------------------------

newtype BB a = BB
  { unBB :: WriterT [BasicBlock] (StateT RW Id) a
  } deriving (Functor,Monad,MonadFix)

instance FreshName BB where
  freshName pfx = BB $ do
    rw <- get
    set $! rw { rwIndex = rwIndex rw + 1 }
    return (showString pfx (shows (rwIndex rw) ""))

runBB :: BB a -> (a,[BasicBlock])
runBB m =
  case runId (runStateT emptyRW (runWriterT (unBB m))) of
    ((a,bbs),rw) -> (a,bbs ++ maybeToList (snd (rwBasicBlock rw)))

data RW = RW
  { rwIndex :: !Int
  , rwLabel :: Maybe Ident
  , rwStmts :: [Stmt]
  } deriving Show

emptyRW :: RW
emptyRW  = RW
  { rwIndex = 0
  , rwLabel = Nothing
  , rwStmts = []
  }

rwBasicBlock :: RW -> (RW,Maybe BasicBlock)
rwBasicBlock rw =
  case rwStmts rw of
    []    -> (rw,Nothing)
    stmts ->
      let rw' = rw { rwLabel = Nothing, rwStmts = [] }
          bb  = BasicBlock (rwLabel rw) stmts
       in (rw',Just bb)

emitStmt :: Stmt -> BB ()
emitStmt stmt = do
  BB $ do
    rw <- get
    set $! rw { rwStmts = rwStmts rw ++ [stmt] }
  when (AST.isTerminator (AST.stmtInstr stmt)) terminateBasicBlock

effect :: Instr -> BB ()
effect  = emitStmt . Effect

observe :: Type -> Instr -> BB (Typed Value)
observe ty i = do
  name <- freshName "r"
  let res = Ident name
  emitStmt (Result res i)
  return (Typed ty (ValIdent res))

-- Basic Blocks ----------------------------------------------------------------

freshLabel :: BB Ident
freshLabel  = Ident `fmap` freshName "L"

-- | Force termination of the current basic block, and start a new one with the
-- given label.
label :: Ident -> BB ()
label l = do
  terminateBasicBlock
  BB $ do
    rw <- get
    set $! rw { rwLabel = Just l }

terminateBasicBlock :: BB ()
terminateBasicBlock  = BB $ do
  rw <- get
  let (rw',bb) = rwBasicBlock rw
  put (maybeToList bb)
  set rw'


-- Type Helpers ----------------------------------------------------------------

iT :: Int32 -> Type
iT  = PrimType . Integer

voidT :: Type
voidT  = PrimType Void


-- Value Helpers ---------------------------------------------------------------

class IsValue a where
  toValue :: a -> Value

instance IsValue Value where
  toValue = id

instance IsValue a => IsValue (Typed a) where
  toValue = toValue . typedValue

instance IsValue Integer where
  toValue = ValInteger

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

int :: Integer -> Value
int  = toValue


-- Instructions ----------------------------------------------------------------

comment :: String -> BB ()
comment str = effect (AST.Comment str)

-- | Emit the ``ret'' instruction and terminate the current basic block.
ret :: IsValue a => Typed a -> BB ()
ret tv = effect (AST.Ret (toValue `fmap` tv))

-- | Emit ``ret void'' and terminate the current basic block.
retVoid :: BB ()
retVoid  = effect AST.RetVoid

jump :: Ident -> BB ()
jump l = effect (AST.Jump l)

br :: IsValue a => Typed a -> Ident -> Ident -> BB ()
br c t f = effect (AST.Br (toValue `fmap` c) t f)

unreachable :: BB ()
unreachable  = effect AST.Unreachable

unwind :: BB ()
unwind  = effect AST.Unwind

binop :: (IsValue a, IsValue b)
      => (Typed Value -> Value -> Instr) -> Typed a -> b -> BB (Typed Value)
binop k l r = observe (typedType l) (k (toValue `fmap` l) (toValue r))

add :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
add  = binop (AST.Arith AST.Add)

fadd :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
fadd  = binop (AST.Arith AST.FAdd)

sub :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
sub  = binop (AST.Arith AST.Sub)

fsub :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
fsub  = binop (AST.Arith AST.FSub)

mul :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
mul  = binop (AST.Arith AST.Mul)

fmul :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
fmul  = binop (AST.Arith AST.FMul)

udiv :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
udiv  = binop (AST.Arith AST.UDiv)

sdiv :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
sdiv  = binop (AST.Arith AST.SDiv)

fdiv :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
fdiv  = binop (AST.Arith AST.FDiv)

urem :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
urem  = binop (AST.Arith AST.URem)

srem :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
srem  = binop (AST.Arith AST.SRem)

frem :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
frem  = binop (AST.Arith AST.FRem)

shl :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
shl  = binop (AST.Bit AST.Shl)

lshr :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
lshr  = binop (AST.Bit AST.Lshr)

ashr :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
ashr  = binop (AST.Bit AST.Ashr)

band :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
band  = binop (AST.Bit AST.And)

bor :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
bor  = binop (AST.Bit AST.Or)

bxor :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
bxor  = binop (AST.Bit AST.Xor)

alloca :: Type -> Maybe (Typed Value) -> Maybe Int -> BB (Typed Value)
alloca ty mb align = observe (PtrTo ty) (AST.Alloca ty es align)
  where
  es = fmap toValue `fmap` mb

load :: IsValue a => Typed a -> BB (Typed Value)
load tv =
  case typedType tv of
    PtrTo ty -> observe ty (AST.Load (toValue `fmap` tv))
    _        -> error "load not given a pointer"

store :: (IsValue a, IsValue b) => a -> Typed b -> BB ()
store a ptr =
  case typedType ptr of
    PtrTo ty -> effect (AST.Store (ty -: a) (toValue `fmap` ptr))
    _        -> error "store not given a pointer"

convop :: IsValue a
       => (Typed Value -> Type -> Instr) -> Typed a -> Type -> BB (Typed Value)
convop k a ty = observe ty (k (toValue `fmap` a) ty)

trunc :: IsValue a => Typed a -> Type -> BB (Typed Value)
trunc  = convop (AST.Conv AST.Trunc)

zext :: IsValue a => Typed a -> Type -> BB (Typed Value)
zext  = convop (AST.Conv AST.ZExt)

sext :: IsValue a => Typed a -> Type -> BB (Typed Value)
sext  = convop (AST.Conv AST.SExt)

fptrunc :: IsValue a => Typed a -> Type -> BB (Typed Value)
fptrunc  = convop (AST.Conv AST.FpTrunc)

fpext :: IsValue a => Typed a -> Type -> BB (Typed Value)
fpext  = convop (AST.Conv AST.FpExt)

fptoui :: IsValue a => Typed a -> Type -> BB (Typed Value)
fptoui  = convop (AST.Conv AST.FpToUi)

fptosi :: IsValue a => Typed a -> Type -> BB (Typed Value)
fptosi  = convop (AST.Conv AST.FpToSi)

uitofp :: IsValue a => Typed a -> Type -> BB (Typed Value)
uitofp  = convop (AST.Conv AST.UiToFp)

sitofp :: IsValue a => Typed a -> Type -> BB (Typed Value)
sitofp  = convop (AST.Conv AST.SiToFp)

ptrtoint :: IsValue a => Typed a -> Type -> BB (Typed Value)
ptrtoint  = convop (AST.Conv AST.PtrToInt)

inttoptr :: IsValue a => Typed a -> Type -> BB (Typed Value)
inttoptr  = convop (AST.Conv AST.IntToPtr)

bitcast :: IsValue a => Typed a -> Type -> BB (Typed Value)
bitcast  = convop (AST.Conv AST.BitCast)

icmp :: (IsValue a, IsValue b) => ICmpOp -> Typed a -> b -> BB (Typed Value)
icmp op l r = observe (iT 1) (AST.ICmp op (toValue `fmap` l) (toValue r))

fcmp :: (IsValue a, IsValue b) => FCmpOp -> Typed a -> b -> BB (Typed Value)
fcmp op l r = observe (iT 1) (AST.FCmp op (toValue `fmap` l) (toValue r))

phi :: Type -> [(Value,Ident)] -> BB (Typed Value)
phi ty vs = observe ty (AST.Phi ty vs)

select :: (IsValue a, IsValue b, IsValue c)
       => Typed a -> Typed b -> Typed c -> BB (Typed Value)
select c t f = observe (typedType t)
             $ AST.Select (toValue `fmap` c) (toValue `fmap` t) (toValue f)

getelementptr :: IsValue a
              => Type -> Typed a -> [Typed Value] -> BB (Typed Value)
getelementptr ty ptr ixs = observe ty (AST.GEP (toValue `fmap` ptr) ixs)

-- | Emit a call instruction, and generate a new variable for its result.
call :: IsValue a => Type -> a -> [Typed Value] -> BB (Typed Value)
call rty sym vs = observe rty (AST.Call False rty (toValue sym) vs)

-- | Emit a call instruction, but don't generate a new variable for its result.
call_ :: IsValue a => Type -> a -> [Typed Value] -> BB ()
call_ rty sym vs = effect (AST.Call False rty (toValue sym) vs)
