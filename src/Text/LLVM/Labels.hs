{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyCase, TypeOperators, FlexibleContexts #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

module Text.LLVM.Labels where

import Text.LLVM.AST
import GHC.Generics

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<$>),Applicative(..))
import Data.Traversable (traverse)
#endif

------------------------------------------------------------------------

-- | Generic implementation of 'relabel' the never provides symbols
genericRelabel ::
  (Applicative m, Generic1 f, GHasLabel (Rep1 f)) =>
  (Maybe Symbol -> a -> m b) -> f a -> m (f b)
genericRelabel f x = to1 <$> grelabel f (from1 x)

-- | Implementation details for 'genericRelabel'
class GHasLabel f where
  grelabel :: Applicative m => (Maybe Symbol -> a -> m b) -> f a -> m (f b)

instance GHasLabel f => GHasLabel (M1 i c f) where
  grelabel f (M1 x) = M1 <$> grelabel f x

instance (GHasLabel f, GHasLabel g) => GHasLabel (f :*: g) where
  grelabel f (x :*: y) = (:*:) <$> grelabel f x <*> grelabel f y

instance (GHasLabel f, GHasLabel g) => GHasLabel (f :+: g) where
  grelabel f (L1 x) = L1 <$> grelabel f x
  grelabel f (R1 x) = R1 <$> grelabel f x

instance GHasLabel U1 where
  grelabel _ U1 = pure U1

instance GHasLabel V1 where
  grelabel _ v1 = case v1 of {}

instance GHasLabel Par1 where
  grelabel f (Par1 x) = Par1 <$> f Nothing x

instance GHasLabel (K1 i a) where
  grelabel _ (K1 a) = pure (K1 a)

instance HasLabel f => GHasLabel (Rec1 f) where
  grelabel f (Rec1 x) = Rec1 <$> relabel f x

instance (Traversable f, GHasLabel g) => GHasLabel (f :.: g) where
  grelabel f (Comp1 x) = Comp1 <$> traverse (grelabel f) x

------------------------------------------------------------------------

class Functor f => HasLabel f where
  -- | Given a function for resolving labels, where the presence of a symbol
  -- denotes a label in a different function, rename all labels in a function.
  relabel :: Applicative m => (Maybe Symbol -> a -> m b) -> f a -> m (f b)

instance HasLabel Stmt' where relabel = genericRelabel

instance HasLabel Instr' where
  relabel _ RetVoid               = pure  RetVoid
  relabel _ Unreachable           = pure  Unreachable
  relabel _ Unwind                = pure  Unwind
  relabel _ (Comment str)         = pure (Comment str)
  relabel f (Ret tv)              = Ret <$> traverse (relabel f) tv
  relabel f (Arith op l r)        = Arith op
                                <$> traverse (relabel f) l
                                <*> relabel f r
  relabel f (Bit op l r)          = Bit op
                                <$> traverse (relabel f) l
                                <*> relabel f r
  relabel f (Conv op l r)         = Conv op <$> traverse (relabel f) l <*> pure r
  relabel f (Call t r n as)       = Call t r
                                <$> relabel f n
                                <*> traverse (traverse (relabel f)) as
  relabel f (Alloca t n a)        = Alloca t
                                <$> traverse (traverse (relabel f)) n
                                <*> pure a
  relabel f (Load a ma)           = Load <$> traverse (relabel f) a <*> pure ma
  relabel f (Store d v ma)        = Store
                                <$> traverse (relabel f) d
                                <*> traverse (relabel f) v
                                <*> pure ma
  relabel f (ICmp op l r)         = ICmp op
                                <$> traverse (relabel f) l
                                <*> relabel f r
  relabel f (FCmp op l r)         = FCmp op
                                <$> traverse (relabel f) l
                                <*> relabel f r
  relabel f (GEP ib a is)         = GEP ib
                                <$> traverse (relabel f) a
                                <*> traverse (traverse (relabel f)) is
  relabel f (Select c l r)        = Select
                                <$> traverse (relabel f) c
                                <*> traverse (relabel f) l <*> relabel f r
  relabel f (ExtractValue a is)   = ExtractValue
                                <$> traverse (relabel f) a
                                <*> pure is
  relabel f (InsertValue a i is)  = InsertValue
                                <$> traverse (relabel f) a
                                <*> traverse (relabel f) i
                                <*> pure is
  relabel f (ShuffleVector a b m) = ShuffleVector
                                <$> traverse (relabel f) a
                                <*> relabel f b
                                <*> traverse (relabel f) m
  relabel f (Jump lab)            = Jump <$> f Nothing lab
  relabel f (Br c l r)            = Br
                                <$> traverse (relabel f) c
                                <*> f Nothing l
                                <*> f Nothing r
  relabel f (Invoke r s as u e)   = Invoke r
                                <$> relabel f s
                                <*> traverse (traverse (relabel f)) as
                                <*> f Nothing u
                                <*> f Nothing e
  relabel f (VaArg al t)          = VaArg
                                <$> traverse (relabel f) al
                                <*> pure t
  relabel f (ExtractElt v i)      = ExtractElt
                                <$> traverse (relabel f) v
                                <*> relabel f i
  relabel f (InsertElt v e i)     = InsertElt
                                <$> traverse (relabel f) v
                                <*> traverse (relabel f) e
                                <*> relabel f i
  relabel f (IndirectBr d ls)     = IndirectBr
                                <$> traverse (relabel f) d
                                <*> traverse (f Nothing) ls
  relabel f (Switch c d ls)       =
    let step (n,i) = (\l -> (n,l)) <$> f Nothing i
     in Switch <$> traverse (relabel f) c <*> f Nothing d <*> traverse step ls
  relabel f (Phi t ls)            =
    let step (a,l) = (,) <$> relabel f a <*> f Nothing l
     in Phi t <$> traverse step ls

  relabel f (LandingPad ty fn c cs) = LandingPad ty
                                  <$> traverse (relabel f) fn
                                  <*> pure c
                                  <*> traverse (relabel f) cs

  relabel f (Resume tv)           = Resume <$> traverse (relabel f) tv

instance HasLabel Clause'                     where relabel = genericRelabel
instance HasLabel Value'                      where relabel = genericRelabel
instance HasLabel ValMd'                      where relabel = genericRelabel
instance HasLabel DebugLoc'                   where relabel = genericRelabel
instance HasLabel DebugInfo'                  where relabel = genericRelabel
instance HasLabel DIDerivedType'              where relabel = genericRelabel
instance HasLabel DISubroutineType'           where relabel = genericRelabel
instance HasLabel DIGlobalVariable'           where relabel = genericRelabel
instance HasLabel DIGlobalVariableExpression' where relabel = genericRelabel
instance HasLabel DILocalVariable'            where relabel = genericRelabel
instance HasLabel DISubprogram'               where relabel = genericRelabel
instance HasLabel DICompositeType'            where relabel = genericRelabel
instance HasLabel DILexicalBlock'             where relabel = genericRelabel
instance HasLabel DICompileUnit'              where relabel = genericRelabel
instance HasLabel DILexicalBlockFile'         where relabel = genericRelabel

-- | Clever instance that actually uses the block name
instance HasLabel ConstExpr' where
  relabel f (ConstBlockAddr t l) = ConstBlockAddr t <$> f (Just t) l
  relabel f x = genericRelabel f x
