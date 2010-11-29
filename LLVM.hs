{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeNaturals #-}
{-# LANGUAGE KindSignatures #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module LLVM where

import Pretty as Pretty

import Control.Applicative (Applicative(..))
import Control.Monad.Fix (MonadFix(..))
import Data.Monoid (Monoid(..))
import MonadLib
import Numeric (showHex)

import GHC.TypeNats


-- Doc Utilities ---------------------------------------------------------------

instance Monoid Doc where
  mempty  = Pretty.empty
  mappend = ($+$)


-- Top-level LLVM Monad --------------------------------------------------------

newtype LLVM a = LLVM
  { unLLVM :: StateT Int (WriterT Doc Id) a
  } deriving (Functor,Applicative,Monad,MonadFix)

runLLVM :: LLVM a -> (a,Doc)
runLLVM (LLVM m) = (a,d)
  where
  ((a,_),d) = runId (runWriterT (runStateT 0 m))

instance WriterM LLVM Doc where
  put = LLVM . put

instance RunWriterM LLVM Doc where
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

instance WriterM (BB r) Doc where
  put = BB . put

instance RunWriterM (BB r) Doc where
  collect m = BB (collect (unBB m))


-- LLVM Types ------------------------------------------------------------------

class IsType a where
  ppType :: a -> Doc

class IsType a => IsPrimType a


-- Void Type -------------------------------------------------------------------

-- instance IsPrimType () fails
instance IsType () where
  ppType _ = text "void"



-- Floatin Point Types ---------------------------------------------------------

instance IsPrimType Float
instance IsType Float where
  ppType _ = text "float"

instance IsPrimType Double
instance IsType Double where
  ppType _ = text "double"



-- Integer types ---------------------------------------------------------------

newtype I (n :: Nat) = I Doc

i :: ((m+1) <= Exp2 n) => Nat m -> Value (I n)
i n = Value (integer (natToInteger n))

intBitSize :: TypeNat n => I n -> Nat n
intBitSize _ = nat

instance Pretty (I n) where
  pp _ (I d) = d

instance TypeNat n => IsType (I n) where
  ppType i = char 'i' <> integer (natToInteger (intBitSize i))

instance TypeNat n => IsPrimType (I n)

instance TypeNat n => IsValue (I n)


-- Array/Vector types ----------------------------------------------------------

data Array (n :: Nat) a

arrayElemNum :: TypeNat m => Array m a -> Nat m
arrayElemNum _ = nat

arrayElemType :: Array m a -> a
arrayElemType _ = error "array element type"

instance (TypeNat n, IsType a) => IsType (Array n a) where
  ppType a = brackets (integer (natToInteger (arrayElemNum a)) <+> char 'x'
                  <+> ppType (arrayElemType a))

-- instance IsPrimType (Array n a) fails


data Vector (n :: Nat) a

vectorElemNum :: TypeNat m => Vector m a -> Nat m
vectorElemNum _ = nat

vectorElemType :: Vector m a -> a
vectorElemType _ = error "vector element type"

instance (TypeNat n, IsPrimType a, 1 <= n) => IsType (Vector n a) where
  ppType a = tris (integer (natToInteger (vectorElemNum a)) <+> char 'x'
                  <+> ppType (vectorElemType a))
    where tris p = char '<' <> p <> char '>'

-- instance IsPrimType (Vector n a) fails






-- Tagged Values ---------------------------------------------------------------

newtype Value a = Value Doc

instance Pretty (Value a) where
  pp _ (Value d) = d

valueType :: Value a -> a
valueType  = error "valueType"

toValue :: IsValue a => a -> Value a
toValue a = Value (ppr a)

ppWithType :: IsValue a => Value a -> Doc
ppWithType v = ppType (valueType v) <+> ppr v

-- | Things with first-class values.
class (IsType a, Pretty a) => IsValue a



-- Pointers --------------------------------------------------------------------

data PtrTo a
  = PtrTo Integer
  | NullPtr

ptrType :: PtrTo a -> a
ptrType  = error "ptrType"

instance Pretty a => Pretty (PtrTo a) where
  pp _ (PtrTo i) = text "0x" <+> text (showHex i "")
  pp _ NullPtr   = text "null"

instance IsType a => IsType (PtrTo a) where
  ppType ptr = ppType (ptrType ptr) <> char '*'

instance (IsType a, Pretty a) => IsValue (PtrTo a) where

-- | Construct a pointer to an arbitrary point in memory.
ptrTo :: IsValue a => Integer -> Value (PtrTo a)
ptrTo  = toValue . PtrTo

-- | Construct the null pointer.
nullPtr :: IsValue a => Value (PtrTo a)
nullPtr  = toValue NullPtr

-- | Allocate some memory on the stack.
alloca :: IsType a => Value (I 32) -> Maybe Int -> BB r (Value (PtrTo a))
alloca n mb = mfix $ \ val ->
  observe $ text "alloca" <+> ppType (ptrType (valueType val))
         <> comma <+> ppWithType n
         <> maybe empty (\a -> comma <+> text "align" <+> int a) mb

load :: IsValue a => Value (PtrTo a) -> BB r (Value a)
load v = observe (text "load" <+> ppWithType v)

store :: IsValue a => Value a -> Value (PtrTo a) -> BB r ()
store a ptr = put (text "store" <+> ppWithType a <> comma <+> ppWithType ptr)


data a :> b = a :> b

class GetElementPtrArgs args where
  gepArgs :: args -> [Doc]

instance GetElementPtrArgs (I 32) where
  gepArgs i = [ppWithType (toValue i)]

instance GetElementPtrArgs tl => GetElementPtrArgs (I 32 :> tl) where
  gepArgs (a :> tl) = ppWithType (toValue a) : gepArgs tl

getelementptr :: (IsValue a, IsValue b, GetElementPtrArgs args)
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
observe :: Doc -> BB r (Value a)
observe d = do
  res <- freshVar
  put (ppr res <+> char '=' <+> d)
  return res


-- Results ---------------------------------------------------------------------

data Res a = Res

resType :: Res a -> a
resType  = error "resType"

retVoid :: BB () ()
retVoid  = put (text "ret void")

ret :: IsValue r => Value r -> BB r ()
ret v = put (text "ret" <+> ppType (valueType v) <+> ppr v)


-- Labels ----------------------------------------------------------------------

newtype Lab = Lab String

instance Pretty Lab where
  pp _ (Lab l) = char '%' <> text l

instance IsType Lab where
  ppType _ = text "label"

newLabel :: BB r Lab
newLabel  = Lab `fmap` BB (freshName "L")

defineLabel :: Lab -> BB r (Value a) -> BB r (Value a,Lab)
defineLabel lab@(Lab l) m = do
  put (text l <> char ':')
  res <- m
  return (res,lab)

br :: Lab -> BB r ()
br l = put (text "br" <+> ppType l <+> ppr l)

condBr :: Value (I 1) -> Lab -> Lab -> BB r ()
condBr b t f = put
             $ text "br" <+> ppWithType b
            <> comma <+> ppType t <+> ppr t
            <> comma <+> ppType f <+> ppr f

phi :: IsValue a => (Value a,Lab) -> (Value a,Lab) -> BB r (Value a)
phi (a,la) (b,lb) =
  observe $ text "phi" <+> ppType (valueType a)
        <+> brackets (ppr a <> comma <+> ppr la)
         <> comma <+> brackets (ppr b <> comma <+> ppr lb)


-- Functions -------------------------------------------------------------------

data Fun f = Fun
  { funSym     :: String
  , funLinkage :: Maybe Linkage
  }

instance Pretty (Fun f) where
  pp _ f = char '@' <> text (funSym f)

instance IsFun f => IsType (Fun f) where
  ppType fun = res <+> parens (commas args)
    where (args,res) = funParts (funType fun)

instance IsFun f => IsValue (Fun f)

class IsFun f where
  funParts :: f -> ([Doc],Doc)

-- | Functions can return things that have no values, like void.
instance IsType a => IsFun (Res a) where
  funParts io = ([], ppType (resType io))

-- | Functions can only take arguments that have first-class values.
instance (IsValue a, IsFun b) => IsFun (a -> b) where
  funParts fun = (ppType (funHead fun) : b, r)
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

instance IsValue a => CallArgs (Res a) (BB r (Value a)) where
  callArgs c as fun = do
    let res  = resType (funType fun)
    let args = reverse as
    observe (text c <+> ppType res <+> ppr fun <> parens (commas args))

instance (IsValue a, CallArgs b r) => CallArgs (a -> b) (Value a -> r) where
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
    put (text c <+> ppType res <+> ppr fun <> parens (commas args))

instance (IsValue a, CallArgs_ b r) => CallArgs_ (a -> b) (Value a -> r) where
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
declare fun =
  put (text "declare" <+> res <+> ppr fun <> parens (commas args))
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

instance Pretty Linkage where
  pp _ Private                  = text "private"
  pp _ LinkerPrivate            = text "linker_private"
  pp _ LinkerPrivateWeak        = text "linker_private_weak"
  pp _ LinkerPrivateWeakDefAuto = text "linker_private_weak_def_auto"
  pp _ Internal                 = text "internal"
  pp _ AvailableExternally      = text "available_externally"
  pp _ Linkonce                 = text "linkonce"
  pp _ Weak                     = text "weak"
  pp _ Common                   = text "common"
  pp _ Appending                = text "appending"
  pp _ ExternWeak               = text "extern_weak"
  pp _ LinkonceODR              = text "linkonce_ddr"
  pp _ WeakODR                  = text "weak_odr"
  pp _ DLLImport                = text "dllimport"
  pp _ DLLExport                = text "dllexport"


class IsFun f => Define k f | k -> f, f -> k where
  defineBody :: k -> f -> LLVM ([Doc],Doc)

instance IsType res => Define (BB res ()) (Res res) where
  defineBody m _ = do
    (_,body) <- collect (unBB m)
    return ([],body)

instance (IsValue a, Define k f) => Define (Value a -> k) (a -> f) where
  defineBody k f = do
    a         <- freshVar
    (as,body) <- defineBody (k a) (funTail f)
    return (ppWithType a:as,body)

defineFun :: (IsFun f, Define k f) => Fun f -> k -> LLVM ()
defineFun f k = mfix $ \f' -> do
  let (_,res) = funParts (funType f)
  (ps,body) <- defineBody k (funType f)
  put $ text "define" <+> res <+> ppr f <> parens (commas ps) <+> char '{'
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

defineNewFun :: Define k f => Maybe Linkage -> k -> LLVM (Fun f)
defineNewFun mb k = do
  f <- newFun mb
  defineFun f k
  return f

defineNewNamedFun :: Define k f => String -> Maybe Linkage -> k -> LLVM (Fun f)
defineNewNamedFun sym mb k = do
  f <- newNamedFun sym mb
  defineFun f k
  return f


-- Tests -----------------------------------------------------------------------

test1 :: Fun (I 32 -> Res (I 32))
test1  = Fun "test1" Nothing

test2 :: Fun (Fun (I 32 -> Res (I 32)) -> I 8 -> Res (I 32))
test2  = Fun "test2" Nothing

test3 = do
  id32 <- defineNewFun Nothing $ \ x -> ret (x :: Value (I 32))
  main <- defineNewFun Nothing $ do
    a <- call id32 (i (nat :: Nat 10))
    b <- call id32 a
    ret a

  return ()

test4 =
  do x <- alloca (i (nat :: Nat 2)) Nothing 
     return (x :: Value (PtrTo (Array 10 (I 32))))

test5 =
  do l1 <- newLabel
     l2 <- newLabel
     condBr (i (nat :: Nat 1)) l1 l2

test6 = snd $ runLLVM $ unBB $
  do x <- alloca (i (nat :: Nat 2)) Nothing 
     return (x :: Value (PtrTo (Vector 10 (I 32))))

test7 = do
  f <- defineNewFun Nothing retVoid
  _ <- defineNewFun Nothing (call_ f >> retVoid)
  return ()


