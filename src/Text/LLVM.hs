{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.LLVM where

import Text.LLVM.AST
    (Module(..),Ident(..),Stmt(..),BasicBlock(..),Typed(..),Value(..),Type(..)
    ,PrimType(..),FloatType(..),Instr)
import qualified Text.LLVM.AST as AST

import Control.Monad.Fix (MonadFix)
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
runLLVM (LLVM m) = fst $ runId $ runStateT 0 $ runWriterT m


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
runBB (BB m) = fst $ runId $ runStateT emptyRW $ runWriterT m

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

startBasicBlock :: Maybe Ident -> BB ()
startBasicBlock mb = BB $ do
  rw <- get
  set $! rw
    { rwLabel = mb
    , rwStmts = []
    }

terminateBasicBlock :: BB ()
terminateBasicBlock  = BB $ do
  rw <- get
  put [BasicBlock (rwLabel rw) (rwStmts rw)]
  set $! rw { rwLabel = Nothing, rwStmts = [] }


-- Instructions ----------------------------------------------------------------

retVoid :: BB ()
retVoid  = effect AST.retVoid

ret :: Typed Value -> BB ()
ret  = effect . AST.ret

add :: Typed Value -> Value -> BB (Typed Value)
add l r = observe (typedType l) (AST.add l r)
