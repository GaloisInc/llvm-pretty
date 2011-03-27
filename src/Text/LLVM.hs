{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.LLVM (
    LLVM()
  , runLLVM

  , freshSymbol
  , define

  , BB()
  , freshLabel

  , iT, voidT
  , IsValue(..), int
  , (=:), (-:)

  , label
  , retVoid
  , ret
  , add
  ) where

import Text.LLVM.AST
    (Module(..),Ident(..),Stmt(..),BasicBlock(..),Typed(..),Value(..),Type(..)
    ,PrimType(..),FloatType(..),Instr,Define(..),emptyModule,Symbol(..))
import qualified Text.LLVM.AST as AST

import Control.Monad.Fix (MonadFix)
import Data.Int (Int32)
import Data.Maybe (maybeToList)
import MonadLib


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

-- | Emit ``ret void'' and terminate the current basic block.
retVoid :: BB ()
retVoid  = effect AST.retVoid

-- | Emit the ``ret'' instruction and terminate the current basic block.
ret :: Typed Value -> BB ()
ret  = effect . AST.ret

-- | Emit the ``add'' instruction.
add :: IsValue a => Typed Value -> a -> BB (Typed Value)
add l r = observe (typedType l) (AST.add (toValue `fmap` l) (toValue r))

call :: IsValue a => Type -> a -> [Typed Value] -> BB (Typed Value)
call rty sym vs = observe rty (AST.call False rty (toValue sym) vs)

call_ :: IsValue a => Type -> a -> [Typed Value] -> BB ()
call_ rty sym vs = effect (AST.call False rty (toValue sym) vs)


-- Tests -----------------------------------------------------------------------

test1 = snd $ runLLVM $ do
  add10 <- defineFresh (iT 10) [iT 10 =: Ident "x"] $ \[x] -> do
    ret =<< add x (int 10)

  define voidT (Symbol "main") [] $ \[] -> do
    call_ (iT 10) add10 [iT 10 -: int 10]
    retVoid
