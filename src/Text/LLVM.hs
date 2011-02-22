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
  ) where

import qualified Text.LLVM.AST as AST

import Control.Applicative (Applicative(..))
import Control.Monad.Fix (MonadFix(..))
import Data.Int (Int8,Int16,Int32,Int64)
import Data.List (intersperse)
import Data.Monoid (Monoid(..))
import MonadLib
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


-- Representable Types ---------------------------------------------------------

class HasType t where
  getType :: t -> AST.Type

instance HasType () where
  getType _ = AST.PrimType AST.Void

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

data Fun f = Fun
  { funSymbol :: AST.Symbol
  }

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


call :: Call f k => Fun f -> k
call  = callBody False []

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


-- Instructions ----------------------------------------------------------------

ret :: HasValues r => Value r -> BB r ()
ret v = emitStmt (AST.Effect (AST.ret (typedValue v)))

retVoid :: BB () ()
retVoid  = emitStmt (AST.Effect AST.retVoid)
