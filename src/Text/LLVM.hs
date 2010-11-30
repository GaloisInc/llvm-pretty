{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.LLVM (
    -- * LLVM Monad
    LLVM()
  , runLLVM

    -- * Basic-block Monad
  , BB()

    -- * LLVM Types
  , LLVMType
  , IsType(getType)

    -- * LLVM First-Class Values
  , Value()
  , HasValues()

    -- * LLVM Literals
  , HasLiterals(toValue)

    -- * LLVM Type Helpers
    -- ** Pointers
  , PtrTo()
  , nullPtr
  , alloca
  , load
  , store

    -- ** getelementptr
  , (:>)((:>))
  , GetElementPtrArgs()
  , getelementptr

    -- * Functions
    -- ** Return Values
  , Res()
  , retVoid
  , ret

    -- ** Labels
  , Lab()
  , newLabel
  , defineLabel
  , br
  , condBr
  , phi

    -- ** Comparisons
  , ICmpOp(..)
  , icmp
  , FCmpOp(..)
  , fcmp

    -- ** Function Symbols
  , Fun(..)
  , IsFun()

    -- ** Calling
  , CallArgs()
  , call, tailCall
  , CallArgs_()
  , call_, tailCall_

    -- ** Declaration
  , declare

    -- ** Definition
  , Linkage(..)
  , Define()
  , define, newNamedFun, newFun, defineFun, defineNamedFun
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad.Fix (MonadFix(..))
import Data.Int (Int8,Int16,Int32,Int64)
import Data.List (intersperse)
import Data.Monoid (Monoid(..))
import MonadLib
import Numeric (showHex)
import Text.PrettyPrint.HughesPJ


-- Pretty Printing -------------------------------------------------------------

commas :: [Doc] -> Doc
commas ds = hcat (intersperse (comma <> space) ds)


-- Programs --------------------------------------------------------------------

newtype Program = P
  { getProgram :: Doc
  } deriving (Show)

instance Monoid Program where
  mempty              = P empty
  mappend (P a) (P b) = P (a $+$ b)

emit :: WriterM m Program => Doc -> m ()
emit d = put (P d)


-- Top-level LLVM Monad --------------------------------------------------------

newtype LLVM a = LLVM
  { unLLVM :: StateT Int (WriterT Program Id) a
  } deriving (Functor,Applicative,Monad,MonadFix)

runLLVM :: LLVM a -> (a,Doc)
runLLVM (LLVM m) = (a,d)
  where
  ((a,_),P d) = runId (runWriterT (runStateT 0 m))

instance WriterM LLVM Program where
  put = LLVM . put

instance RunWriterM LLVM Program where
  collect m = LLVM (collect (unLLVM m))

freshName :: String -> LLVM String
freshName pfx = LLVM $ do
  i <- get
  set $! i + 1
  return (pfx ++ show i)


-- Basic Block Monad -----------------------------------------------------------

newtype BB r a = BB
  { unBB :: LLVM a
  } deriving (Functor,Applicative,Monad,MonadFix)

instance WriterM (BB r) Program where
  put = BB . put

instance RunWriterM (BB r) Program where
  collect m = BB (collect (unBB m))


-- LLVM Types ------------------------------------------------------------------

data LLVMType
  = Void
  | Integer Int32
  | Float
  | Double
  | PtrTo LLVMType
  | Label
  | FunTy [LLVMType] LLVMType

ppLLVMType :: LLVMType -> Doc
ppLLVMType Void         = text "void"
ppLLVMType (Integer i)  = char 'i' <> integer (toInteger i)
ppLLVMType Float        = text "float"
ppLLVMType Double       = text "double"
ppLLVMType (PtrTo ty)   = ppLLVMType ty <> char '*'
ppLLVMType Label        = text "label"
ppLLVMType (FunTy as r) = ppLLVMType r <> parens (commas (map ppLLVMType as))

class IsType a where
  getType :: a -> LLVMType

ppType :: IsType a => a -> Doc
ppType  = ppLLVMType . getType

instance IsType () where
  getType _ = Void

instance IsType Bool where
  getType _ = Integer 1

instance IsType Int8 where
  getType _ = Integer 8

instance IsType Int16 where
  getType _ = Integer 16

instance IsType Int32 where
  getType _ = Integer 32

instance IsType Int64 where
  getType _ = Integer 64

instance IsType Float where
  getType _ = Float

instance IsType Double where
  getType _ = Double


-- First-class Values ----------------------------------------------------------

newtype Value a = Value { ppv :: Doc }

valueType :: Value a -> a
valueType  = error "valueType"

ppWithType :: HasValues a => Value a -> Doc
ppWithType v = ppType (valueType v) <+> ppv v

-- | Things with first-class values.
class IsType a => HasValues a

instance HasValues Bool
instance HasValues Int8
instance HasValues Int16
instance HasValues Int32
instance HasValues Int64


-- Literals --------------------------------------------------------------------

ppl :: HasLiterals a => a -> Doc
ppl  = ppv . toValue

class HasValues a => HasLiterals a where
  toValue :: a -> Value a

instance HasLiterals Bool where
  toValue False = Value (int 0)
  toValue True  = Value (int 1)

instance HasLiterals Int8 where
  toValue = Value . integer . toInteger

instance HasLiterals Int16 where
  toValue = Value . integer . toInteger

instance HasLiterals Int32 where
  toValue = Value . integer . toInteger

instance HasLiterals Int64 where
  toValue = Value . integer . toInteger


-- Pointers --------------------------------------------------------------------

data PtrTo a

ptrType :: PtrTo a -> a
ptrType  = error "ptrType"

instance IsType a => IsType (PtrTo a) where
  getType = PtrTo . getType . ptrType

instance IsType a => HasValues (PtrTo a)

-- | Construct the null pointer.
nullPtr :: HasValues a => Value (PtrTo a)
nullPtr  = Value (text "null")

-- | Allocate some memory on the stack.
alloca :: IsType a => Value Int32 -> Maybe Int -> BB r (Value (PtrTo a))
alloca n mb = mfix $ \ val ->
  observe $ text "alloca" <+> ppType (ptrType (valueType val))
         <> comma <+> ppWithType n
         <> maybe empty (\a -> comma <+> text "align" <+> int a) mb

load :: HasValues a => Value (PtrTo a) -> BB r (Value a)
load v = observe (text "load" <+> ppWithType v)

store :: HasValues a => Value a -> Value (PtrTo a) -> BB r ()
store a ptr = emit (text "store" <+> ppWithType a <> comma <+> ppWithType ptr)


data a :> b = a :> b

class GetElementPtrArgs args where
  gepArgs :: args -> [Doc]

instance GetElementPtrArgs Int32 where
  gepArgs i = [ppWithType (toValue i)]

instance GetElementPtrArgs tl => GetElementPtrArgs (Int32 :> tl) where
  gepArgs (a :> tl) = ppWithType (toValue a) : gepArgs tl

getelementptr :: (IsType a, IsType b, GetElementPtrArgs args)
              => Value (PtrTo a) -> args -> BB r (Value (PtrTo b))
getelementptr ptr idx =
  observe $ text "getelementptr" <+> ppWithType ptr <> comma
        <+> commas (gepArgs idx)


-- Variables -------------------------------------------------------------------

class FreshVar m where
  freshVar :: m (Value a)

instance FreshVar LLVM where
  freshVar = do
    n <- freshName "var"
    return (Value (char '%' <> text n))

instance FreshVar (BB r) where
  freshVar = BB freshVar

-- | DO NOT EXPORT.
-- observe is used for naming results from internal primitives.
observe :: IsType a => Doc -> BB r (Value a)
observe d = do
  res <- freshVar
  emit (ppv res <+> char '=' <+> d)
  return res


-- Results ---------------------------------------------------------------------

data Res a = Res

resType :: Res a -> a
resType  = error "resType"

retVoid :: BB () ()
retVoid  = emit (text "ret void")

ret :: HasValues r => Value r -> BB r ()
ret v = emit (text "ret" <+> ppType (valueType v) <+> ppv v)


-- Labels ----------------------------------------------------------------------

newtype Lab = Lab String

ppLab :: Lab -> Doc
ppLab (Lab l) = char '%' <> text l

instance IsType Lab where
  getType _ = Label

newLabel :: BB r Lab
newLabel  = Lab `fmap` BB (freshName "L")

defineLabel :: Lab -> BB r (Value a) -> BB r (Value a,Lab)
defineLabel lab@(Lab l) m = do
  emit (text l <> char ':')
  res <- m
  return (res,lab)

br :: Lab -> BB r ()
br l = emit (text "br" <+> ppType l <+> ppLab l)

condBr :: Value Bool -> Lab -> Lab -> BB r ()
condBr b t f = emit
             $ text "br" <+> ppWithType b
            <> comma <+> ppType t <+> ppLab t
            <> comma <+> ppType f <+> ppLab f

phi :: HasValues a => (Value a,Lab) -> (Value a,Lab) -> BB r (Value a)
phi (a,la) (b,lb) =
  observe $ text "phi" <+> ppType (valueType a)
        <+> brackets (ppv a <> comma <+> ppLab la)
         <> comma <+> brackets (ppv b <> comma <+> ppLab lb)


-- Comparisons -----------------------------------------------------------------

data ICmpOp
  = Ieq  | Ine | Iugt | Iuge | Ilt
  | Ile | Isgt | Isge | Islt | Isle
    deriving Show

ppICmpOp :: ICmpOp -> Doc
ppICmpOp Ieq  = text "eq"
ppICmpOp Ine  = text "ne"
ppICmpOp Iugt = text "ugt"
ppICmpOp Iuge = text "uge"
ppICmpOp Ilt  = text "lt"
ppICmpOp Ile  = text "le"
ppICmpOp Isgt = text "sgt"
ppICmpOp Isge = text "sge"
ppICmpOp Islt = text "slt"
ppICmpOp Isle = text "sle"

icmp :: HasValues a => ICmpOp -> Value a -> Value a -> BB r (Value Bool)
icmp op x y = observe
            $ text "icmp" <+> ppICmpOp op <+> ppWithType x <> comma <+> ppv y


data FCmpOp
  = Ffalse | Ftrue
  | Foeq   | Fogt | Foge | Folt | Fole | Fone | Ford
  | Fueq   | Fugt | Fuge | Fult | Fule | Fune | Funo
    deriving Show

ppFCmpOp :: FCmpOp -> Doc
ppFCmpOp Ffalse = text "false"
ppFCmpOp Ftrue  = text "true"
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

fcmp :: HasValues a => FCmpOp -> Value a -> Value a -> BB r (Value Bool)
fcmp op x y = observe
            $ text "fcmp" <+> ppFCmpOp op <+> ppWithType x <> comma <+> ppv y

-- Functions -------------------------------------------------------------------

data Fun f = Fun
  { funSym     :: String
  , funLinkage :: Maybe Linkage
  }

instance IsFun f => IsType (Fun f) where
  getType = uncurry FunTy . funParts . funType

instance IsFun f => HasValues (Fun f)

instance IsFun f => HasLiterals (Fun f) where
  toValue f = Value (char '@' <> text (funSym f))

class IsFun f where
  funParts :: f -> ([LLVMType],LLVMType)

-- | Functions can return things that have no values, like void.
instance IsType a => IsFun (Res a) where
  funParts res = ([], getType (resType res))

-- | Functions can only take arguments that have first-class values.
instance (HasValues a, IsFun b) => IsFun (a -> b) where
  funParts fun = (getType (funHead fun) : b, r)
    where (b,r) = funParts (funTail fun)

funType :: Fun f -> f
funType  = error "funType"

funHead :: (a -> b) -> a
funHead  = error "funHead"

funTail :: (a -> b) -> b
funTail  = error "funTail"

setFunType :: b -> Fun a -> Fun b
setFunType _ f = Fun
  { funSym     = funSym f
  , funLinkage = funLinkage f
  }


-- Function Calls --------------------------------------------------------------

class CallArgs f k | f -> k, k -> f where
  callArgs :: String -> [Doc] -> Fun f -> k

instance HasValues a => CallArgs (Res a) (BB r (Value a)) where
  callArgs c as fun = do
    let res  = resType (funType fun)
    let args = reverse as
    observe (text c <+> ppType res <+> ppl fun <> parens (commas args))

instance (HasValues a, CallArgs b r) => CallArgs (a -> b) (Value a -> r) where
  callArgs c as fun a = callArgs c (arg:as) (setFunType (funTail f) fun)
    where
    f   = funType fun
    arg = ppWithType a

-- | Call a function that returns a first-class value.
call :: CallArgs f k => Fun f -> k
call  = callArgs "call" []

-- | Call a function that returns a first-class value, signaling the optimizer
-- that this call site is a possible tail call optimization.
tailCall :: CallArgs f k => Fun f -> k
tailCall  = callArgs "tail call" []


-- Procedure Calls -------------------------------------------------------------

class CallArgs_ f k | f -> k, k -> f where
  callArgs_ :: String -> [Doc] -> Fun f -> k

instance IsType a => CallArgs_ (Res a) (BB r ()) where
  callArgs_ c as fun = do
    let res  = resType (funType fun)
    let args = reverse as
    emit (text c <+> ppType res <+> ppl fun <> parens (commas args))

instance (HasValues a, CallArgs_ b r) => CallArgs_ (a -> b) (Value a -> r) where
  callArgs_ c as fun a = callArgs_ c (arg:as) (setFunType (funTail f) fun)
    where
    f   = funType fun
    arg = ppWithType a

-- | Call a function, ignoring its result.
call_ :: CallArgs_ f k => Fun f -> k
call_  = callArgs_ "call" []

-- | Call a function ignoring its result, signaling the optimizer that this call
-- site is a possible tail call optimization.
tailCall_ :: CallArgs_ f k => Fun f -> k
tailCall_  = callArgs_ "tail call" []


-- Function Declaration --------------------------------------------------------

declare :: IsFun f => Fun f -> LLVM ()
declare fun = emit
            $ text "declare" <+> ppLLVMType res <+> ppl fun
           <> parens (commas (map ppLLVMType args))
  where
  (args,res) = funParts (funType fun)


-- Function Definition ---------------------------------------------------------

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
  | DLLImport
  | DLLExport

ppLinkage :: Linkage -> Doc
ppLinkage Private                  = text "private"
ppLinkage LinkerPrivate            = text "linker_private"
ppLinkage LinkerPrivateWeak        = text "linker_private_weak"
ppLinkage LinkerPrivateWeakDefAuto = text "linker_private_weak_def_auto"
ppLinkage Internal                 = text "internal"
ppLinkage AvailableExternally      = text "available_externally"
ppLinkage Linkonce                 = text "linkonce"
ppLinkage Weak                     = text "weak"
ppLinkage Common                   = text "common"
ppLinkage Appending                = text "appending"
ppLinkage ExternWeak               = text "extern_weak"
ppLinkage LinkonceODR              = text "linkonce_ddr"
ppLinkage WeakODR                  = text "weak_odr"
ppLinkage DLLImport                = text "dllimport"
ppLinkage DLLExport                = text "dllexport"


class IsFun f => Define k f | k -> f, f -> k where
  defineBody :: k -> f -> LLVM ([Doc],Doc)

instance IsType res => Define (BB res ()) (Res res) where
  defineBody m _ = do
    (_,body) <- collect (unBB m)
    return ([],getProgram body)

instance (HasValues a, Define k f) => Define (Value a -> k) (a -> f) where
  defineBody k f = do
    a         <- freshVar
    (as,body) <- defineBody (k a) (funTail f)
    return (ppWithType a:as,body)

define :: (IsFun f, Define k f) => Fun f -> k -> LLVM ()
define f k = mfix $ \f' -> do
  let (_,res) = funParts (funType f)
      linkage = maybe empty ppLinkage (funLinkage f)
  (ps,body) <- defineBody k (funType f)
  emit $ text "define" <+> linkage <+> ppLLVMType res <+> ppl f
      <> parens (commas ps)
     <+> char '{'
     $+$ nest 2 body
     $+$ char '}'
  return ()

newNamedFun :: IsFun f => String -> Maybe Linkage -> LLVM (Fun f)
newNamedFun sym mb = return Fun
  { funSym     = sym
  , funLinkage = mb
  }

newFun :: IsFun f => Maybe Linkage -> LLVM (Fun f)
newFun mb = do
  sym <- freshName "fun"
  newNamedFun sym mb

defineFun :: Define k f => Maybe Linkage -> k -> LLVM (Fun f)
defineFun mb k = do
  f <- newFun mb
  define f k
  return f

defineNamedFun :: Define k f => String -> Maybe Linkage -> k -> LLVM (Fun f)
defineNamedFun sym mb k = do
  f <- newNamedFun sym mb
  define f k
  return f


-- Tests -----------------------------------------------------------------------

test1 :: Fun (Int32 -> Res Int32)
test1  = Fun "test1" Nothing

test2 :: Fun (Fun (Int32 -> Res Int32) -> Int8 -> Res Int32)
test2  = Fun "test2" Nothing

test3 = do
  id32 <- defineFun (Just Private) $ \ x -> ret (x :: Value Int32)
  main <- defineNamedFun "main" Nothing $ do
    a <- call id32 (toValue 10)
    b <- call id32 a
    ret a

  return ()

test6 = do
  f <- defineFun Nothing retVoid
  _ <- defineFun Nothing (call_ f >> retVoid)
  return ()
