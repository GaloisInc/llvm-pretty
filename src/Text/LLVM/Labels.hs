{-# LANGUAGE CPP #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

module Text.LLVM.Labels where

import Text.LLVM.AST

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<$>),Applicative(..))
import Data.Traversable (traverse)
#endif

class Functor f => HasLabel f where
  -- | Given a function for resolving labels, where the presence of a symbol
  -- denotes a label in a different function, rename all labels in a function.
  relabel :: Applicative m => (Maybe Symbol -> a -> m b) -> f a -> m (f b)

instance HasLabel Stmt' where
  relabel f stmt = case stmt of
    Result r i mds -> Result r <$> relabel f i <*> traverse relabelMd mds
    Effect i mds   -> Effect   <$> relabel f i <*> traverse relabelMd mds
    where
    relabelMd (str,md) = (\md' -> (str,md')) `fmap` relabel f md

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

instance HasLabel Clause' where
  relabel f clause = case clause of
    Catch  tv -> Catch  <$> traverse (relabel f) tv
    Filter tv -> Filter <$> traverse (relabel f) tv

instance HasLabel Value' where
  relabel _ (ValInteger i)       = pure (ValInteger i)
  relabel _ (ValBool b)          = pure (ValBool b)
  relabel _ (ValFloat f)         = pure (ValFloat f)
  relabel _ (ValDouble d)        = pure (ValDouble d)
  relabel _ (ValIdent i)         = pure (ValIdent i)
  relabel _ (ValSymbol s)        = pure (ValSymbol s)
  relabel _ (ValString str)      = pure (ValString str)
  relabel _  ValUndef            = pure ValUndef
  relabel _  ValNull             = pure ValNull
  relabel _  ValZeroInit         = pure ValZeroInit
  relabel _ (ValAsm s a i c)     = pure (ValAsm s a i c)
  relabel f (ValMd m)            = ValMd <$> relabel f m
  relabel f (ValArray t es)      = ValArray t <$> traverse (relabel f) es
  relabel f (ValVector pt es)    = ValVector pt <$> traverse (relabel f) es
  relabel f (ValStruct fs)       = ValStruct <$> traverse (traverse (relabel f)) fs
  relabel f (ValConstExpr ce)    = ValConstExpr <$> relabel f ce
  relabel f (ValLabel lab)       = ValLabel <$> f Nothing lab
  relabel f (ValPackedStruct es) =
    ValPackedStruct <$> traverse (traverse (relabel f)) es

instance HasLabel ValMd' where
  relabel f md = case md of
    ValMdString str -> pure (ValMdString str)
    ValMdValue tv   -> ValMdValue <$> traverse (relabel f) tv
    ValMdRef i      -> pure (ValMdRef i)
    ValMdNode es    -> ValMdNode <$> traverse (traverse (relabel f)) es
    ValMdLoc dl     -> ValMdLoc <$> relabel f dl
    ValMdFile df    -> pure (ValMdFile df)

instance HasLabel DebugLoc' where
  relabel f dl = upd <$> relabel f (dlScope dl)
                     <*> traverse (relabel f) (dlIA dl)
    where
    upd scope ia = dl
      { dlScope = scope
      , dlIA    = ia
      }

instance HasLabel ConstExpr' where
  relabel f (ConstGEP inb is)   = ConstGEP inb
                              <$> traverse (traverse (relabel f)) is
  relabel f (ConstConv op a t)  = ConstConv op
                              <$> traverse (relabel f) a
                              <*> pure t
  relabel f (ConstSelect c l r) = ConstSelect
                              <$> traverse (relabel f) c
                              <*> traverse (relabel f) l
                              <*> traverse (relabel f) r
  relabel f (ConstBlockAddr t l)= ConstBlockAddr t
                              <$> f (Just t) l
