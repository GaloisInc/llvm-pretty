{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DoRec #-}

module Text.LLVM (
    -- * LLVM Monad
    LLVM()
  , runLLVM
  , emitTypeDecl
  , emitGlobal
  , emitDeclare
  , emitDefine

    -- * Alias Introduction
  , alias

    -- * Function Definition
  , freshSymbol
  , define
  , defineFresh
  , declare
  , global

    -- * Types
  , iT, ptrT, voidT
  , (=:), (-:)

    -- * Values
  , IsValue(..)
  , int
  , struct
  , array

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
  , getelementptr
  , nullPtr

    -- * Other Operations
  , icmp
  , fcmp
  , phi
  , select
  , call, call_

    -- * Re-exported
  , module Text.LLVM.AST
  ) where

import Text.LLVM.AST

import Control.Monad.Fix (MonadFix)
import Data.Char (ord)
import Data.Int (Int32)
import Data.Maybe (maybeToList)
import MonadLib hiding (jump,Label)
import qualified Data.Map as Map


-- Fresh Names -----------------------------------------------------------------

type Names = Map.Map String Int

nextName :: String -> Names -> (String,Names)
nextName pfx ns =
  case Map.lookup pfx ns of
    Nothing -> (fmt 1,  Map.insert pfx 2 ns)
    Just ix -> (fmt ix, Map.insert pfx (ix+1) ns)
  where
  fmt i = showString pfx (shows i "")


-- LLVM Monad ------------------------------------------------------------------

newtype LLVM a = LLVM
  { unLLVM :: WriterT Module (StateT Names Id) a
  } deriving (Functor,Monad,MonadFix)

freshNameLLVM :: String -> LLVM String
freshNameLLVM pfx = LLVM $ do
  ns <- get
  let (n,ns') = nextName pfx ns
  set ns'
  return n

runLLVM :: LLVM a -> (a,Module)
runLLVM  = fst . runId . runStateT Map.empty . runWriterT . unLLVM

emitTypeDecl :: TypeDecl -> LLVM ()
emitTypeDecl td = LLVM (put emptyModule { modTypes = [td] })

emitGlobal :: Global -> LLVM ()
emitGlobal g = LLVM (put emptyModule { modGlobals = [g] })

emitDefine :: Define -> LLVM ()
emitDefine d = LLVM (put emptyModule { modDefines = [d] })

emitDeclare :: Declare -> LLVM ()
emitDeclare d = LLVM (put emptyModule { modDeclares = [d] })

alias :: Ident -> Type -> LLVM ()
alias i ty = emitTypeDecl (TypeDecl i ty)

freshSymbol :: LLVM Symbol
freshSymbol  = Symbol `fmap` freshNameLLVM "f"

define :: FunAttrs -> Type -> Symbol -> [Type] -> ([Typed Value] -> BB ())
       -> LLVM Value
define attrs rty fun tys body = do
  names <- replicateM (length tys) (freshNameLLVM "a")
  let args = zipWith (\ty n -> Typed ty (Ident n)) tys names
  emitDefine Define
    { defAttrs   = attrs
    , defName    = fun
    , defRetType = rty
    , defArgs    = args
    , defBody    = snd (runBB (body (map (fmap toValue) args)))
    }
  return (ValSymbol fun)

-- | A combination of define and @freshSymbol@.
defineFresh :: FunAttrs -> Type -> [Type] -> ([Typed Value] -> BB ())
            -> LLVM Value
defineFresh attrs rty args body = do
  sym <- freshSymbol
  define attrs rty sym args body


-- | Emit a declaration.
declare :: Type -> Symbol -> [Type] -> LLVM ()
declare rty sym tys = emitDeclare Declare
  { decRetType = rty
  , decName    = sym
  , decArgs    = tys
  }


-- | Emit a global declaration.
global :: Symbol -> Typed Value -> LLVM ()
global sym val = emitGlobal Global
  { globalSym   = sym
  , globalType  = typedType val
  , globalValue = typedValue val
  }


-- | Output a somewhat clunky representation for a string global, that deals
-- well with escaping in the haskell-source string.
string :: Symbol -> String -> LLVM ()
string sym str = global sym (array (iT 8) bytes)
  where
  bytes = [ int (fromIntegral (ord c)) | c <- str ]


-- Basic Block Monad -----------------------------------------------------------

newtype BB a = BB
  { unBB :: WriterT [BasicBlock] (StateT RW Id) a
  } deriving (Functor,Monad,MonadFix)

freshNameBB :: String -> BB String
freshNameBB pfx = BB $ do
  rw <- get
  let (n,ns') = nextName pfx (rwNames rw)
  set rw { rwNames = ns' }
  return n

runBB :: BB a -> (a,[BasicBlock])
runBB m =
  case runId (runStateT emptyRW (runWriterT (unBB m))) of
    ((a,bbs),rw) -> (a,bbs ++ maybeToList (snd (rwBasicBlock rw)))

data RW = RW
  { rwNames :: Names
  , rwLabel :: Maybe Ident
  , rwStmts :: [Stmt]
  } deriving Show

emptyRW :: RW
emptyRW  = RW
  { rwNames = Map.empty
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
  when (isTerminator (stmtInstr stmt)) terminateBasicBlock

effect :: Instr -> BB ()
effect  = emitStmt . Effect

observe :: Type -> Instr -> BB (Typed Value)
observe ty i = do
  name <- freshNameBB ""
  let res = Ident name
  emitStmt (Result res i)
  return (Typed ty (ValIdent res))

-- Basic Blocks ----------------------------------------------------------------

freshLabel :: BB Ident
freshLabel  = Ident `fmap` freshNameBB "L"

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

ptrT :: Type -> Type
ptrT  = PtrTo

voidT :: Type
voidT  = PrimType Void

arrayT :: Int32 -> Type -> Type
arrayT  = Array


-- Value Helpers ---------------------------------------------------------------

class IsValue a where
  toValue :: a -> Value

instance IsValue Value where
  toValue = id

instance IsValue a => IsValue (Typed a) where
  toValue = toValue . typedValue

instance IsValue Integer where
  toValue = ValInteger

instance IsValue Int where
  toValue = ValInteger . toInteger

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

comment :: String -> BB ()
comment str = effect (Comment str)

-- | Emit the ``ret'' instruction and terminate the current basic block.
ret :: IsValue a => Typed a -> BB ()
ret tv = effect (Ret (toValue `fmap` tv))

-- | Emit ``ret void'' and terminate the current basic block.
retVoid :: BB ()
retVoid  = effect RetVoid

jump :: Ident -> BB ()
jump l = effect (Jump l)

br :: IsValue a => Typed a -> Ident -> Ident -> BB ()
br c t f = effect (Br (toValue `fmap` c) t f)

unreachable :: BB ()
unreachable  = effect Unreachable

unwind :: BB ()
unwind  = effect Unwind

binop :: (IsValue a, IsValue b)
      => (Typed Value -> Value -> Instr) -> Typed a -> b -> BB (Typed Value)
binop k l r = observe (typedType l) (k (toValue `fmap` l) (toValue r))

add :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
add  = binop (Arith Add)

fadd :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
fadd  = binop (Arith FAdd)

sub :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
sub  = binop (Arith Sub)

fsub :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
fsub  = binop (Arith FSub)

mul :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
mul  = binop (Arith Mul)

fmul :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
fmul  = binop (Arith FMul)

udiv :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
udiv  = binop (Arith UDiv)

sdiv :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
sdiv  = binop (Arith SDiv)

fdiv :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
fdiv  = binop (Arith FDiv)

urem :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
urem  = binop (Arith URem)

srem :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
srem  = binop (Arith SRem)

frem :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
frem  = binop (Arith FRem)

shl :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
shl  = binop (Bit Shl)

lshr :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
lshr  = binop (Bit Lshr)

ashr :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
ashr  = binop (Bit Ashr)

band :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
band  = binop (Bit And)

bor :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
bor  = binop (Bit Or)

bxor :: (IsValue a, IsValue b) => Typed a -> b -> BB (Typed Value)
bxor  = binop (Bit Xor)

alloca :: Type -> Maybe (Typed Value) -> Maybe Int -> BB (Typed Value)
alloca ty mb align = observe (PtrTo ty) (Alloca ty es align)
  where
  es = fmap toValue `fmap` mb

load :: IsValue a => Typed a -> BB (Typed Value)
load tv =
  case typedType tv of
    PtrTo ty -> observe ty (Load (toValue `fmap` tv))
    _        -> error "load not given a pointer"

store :: (IsValue a, IsValue b) => a -> Typed b -> BB ()
store a ptr =
  case typedType ptr of
    PtrTo ty -> effect (Store (ty -: a) (toValue `fmap` ptr))
    _        -> error "store not given a pointer"

nullPtr :: Type -> Typed Value
nullPtr ty = ptrT ty =: ValNull

convop :: IsValue a
       => (Typed Value -> Type -> Instr) -> Typed a -> Type -> BB (Typed Value)
convop k a ty = observe ty (k (toValue `fmap` a) ty)

trunc :: IsValue a => Typed a -> Type -> BB (Typed Value)
trunc  = convop (Conv Trunc)

zext :: IsValue a => Typed a -> Type -> BB (Typed Value)
zext  = convop (Conv ZExt)

sext :: IsValue a => Typed a -> Type -> BB (Typed Value)
sext  = convop (Conv SExt)

fptrunc :: IsValue a => Typed a -> Type -> BB (Typed Value)
fptrunc  = convop (Conv FpTrunc)

fpext :: IsValue a => Typed a -> Type -> BB (Typed Value)
fpext  = convop (Conv FpExt)

fptoui :: IsValue a => Typed a -> Type -> BB (Typed Value)
fptoui  = convop (Conv FpToUi)

fptosi :: IsValue a => Typed a -> Type -> BB (Typed Value)
fptosi  = convop (Conv FpToSi)

uitofp :: IsValue a => Typed a -> Type -> BB (Typed Value)
uitofp  = convop (Conv UiToFp)

sitofp :: IsValue a => Typed a -> Type -> BB (Typed Value)
sitofp  = convop (Conv SiToFp)

ptrtoint :: IsValue a => Typed a -> Type -> BB (Typed Value)
ptrtoint  = convop (Conv PtrToInt)

inttoptr :: IsValue a => Typed a -> Type -> BB (Typed Value)
inttoptr  = convop (Conv IntToPtr)

bitcast :: IsValue a => Typed a -> Type -> BB (Typed Value)
bitcast  = convop (Conv BitCast)

icmp :: (IsValue a, IsValue b) => ICmpOp -> Typed a -> b -> BB (Typed Value)
icmp op l r = observe (iT 1) (ICmp op (toValue `fmap` l) (toValue r))

fcmp :: (IsValue a, IsValue b) => FCmpOp -> Typed a -> b -> BB (Typed Value)
fcmp op l r = observe (iT 1) (FCmp op (toValue `fmap` l) (toValue r))

phi :: Type -> [(Value,Ident)] -> BB (Typed Value)
phi ty vs = observe ty (Phi ty vs)

select :: (IsValue a, IsValue b, IsValue c)
       => Typed a -> Typed b -> Typed c -> BB (Typed Value)
select c t f = observe (typedType t)
             $ Select (toValue `fmap` c) (toValue `fmap` t) (toValue f)

getelementptr :: IsValue a
              => Type -> Typed a -> [Typed Value] -> BB (Typed Value)
getelementptr ty ptr ixs = observe ty (GEP (toValue `fmap` ptr) ixs)

-- | Emit a call instruction, and generate a new variable for its result.
call :: IsValue a => Type -> a -> [Typed Value] -> BB (Typed Value)
call rty sym vs = observe rty (Call False rty (toValue sym) vs)

-- | Emit a call instruction, but don't generate a new variable for its result.
call_ :: IsValue a => Type -> a -> [Typed Value] -> BB ()
call_ rty sym vs = effect (Call False rty (toValue sym) vs)


-- Tests -----------------------------------------------------------------------

test1 = snd $ runLLVM $ do
  fact <- defineFresh emptyFunAttrs (iT 32) [iT 32] $ \[a] -> do
    check <- freshLabel
    body  <- freshLabel
    done  <- freshLabel

    rec label check
        acc <- phi (iT 32) [(int 0,check),    (toValue acc',body)]
        i   <- phi (iT 32) [(toValue a,check),(toValue i',body)]
        b   <- icmp Iugt i (int 0)
        br b body done

        label body
        acc' <- mul acc i
        i'   <- sub i (int 1)
        jump check

    label done
    ret acc

  return ()
