{-# LANGUAGE CPP #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

module Text.LLVM.Labels where

import Text.LLVM.AST

import qualified Data.Traversable as T

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((<$>),Applicative(..))
#endif

class Functor f => HasLabel f where
  -- | Given a function for resolving labels, where the presence of a symbol
  -- denotes a label in a different function, rename all labels in a function.
  relabel :: (Applicative m, Monad m)
          => (Maybe Symbol -> a -> m b) -> f a -> m (f b)

instance HasLabel Stmt' where
  relabel f stmt = case stmt of
    Result r i mds -> Result r <$> relabel f i <*> T.mapM relabelMd mds
    Effect i mds   -> Effect   <$> relabel f i <*> T.mapM relabelMd mds
    where
    relabelMd (str,md) = (\md' -> (str,md')) `fmap` relabel f md

instance HasLabel Instr' where
  relabel _ RetVoid               = return  RetVoid
  relabel _ Unreachable           = return  Unreachable
  relabel _ Unwind                = return  Unwind
  relabel _ (Comment str)         = return (Comment str)
  relabel f (Ret tv)              = Ret <$> T.mapM (relabel f) tv
  relabel f (Arith op l r)        = Arith op
                                <$> T.mapM (relabel f) l
                                <*> relabel f r
  relabel f (Bit op l r)          = Bit op
                                <$> T.mapM (relabel f) l
                                <*> relabel f r
  relabel f (Conv op l r)         = Conv op <$> T.mapM (relabel f) l <*> pure r
  relabel f (Call t r n as)       = Call t r
                                <$> relabel f n
                                <*> T.mapM (T.mapM (relabel f)) as
  relabel f (Alloca t n a)        = Alloca t
                                <$> T.mapM (T.mapM (relabel f)) n
                                <*> pure a
  relabel f (Load a ma)           = Load <$> T.mapM (relabel f) a <*> pure ma
  relabel f (Store d v ma)        = Store
                                <$> T.mapM (relabel f) d
                                <*> T.mapM (relabel f) v
                                <*> pure ma
  relabel f (ICmp op l r)         = ICmp op
                                <$> T.mapM (relabel f) l
                                <*> relabel f r
  relabel f (FCmp op l r)         = FCmp op
                                <$> T.mapM (relabel f) l
                                <*> relabel f r
  relabel f (GEP ib a is)         = GEP ib
                                <$> T.mapM (relabel f) a
                                <*> T.mapM (T.mapM (relabel f)) is
  relabel f (Select c l r)        = Select
                                <$> T.mapM (relabel f) c
                                <*> T.mapM (relabel f) l <*> relabel f r
  relabel f (ExtractValue a is)   = ExtractValue
                                <$> T.mapM (relabel f) a
                                <*> pure is
  relabel f (InsertValue a i is)  = InsertValue
                                <$> T.mapM (relabel f) a
                                <*> T.mapM (relabel f) i
                                <*> pure is
  relabel f (ShuffleVector a b m) = ShuffleVector
                                <$> T.mapM (relabel f) a
                                <*> relabel f b
                                <*> T.mapM (relabel f) m
  relabel f (Jump lab)            = Jump <$> f Nothing lab
  relabel f (Br c l r)            = Br
                                <$> T.mapM (relabel f) c
                                <*> f Nothing l
                                <*> f Nothing r
  relabel f (Invoke r s as u e)   = Invoke r
                                <$> relabel f s
                                <*> T.mapM (T.mapM (relabel f)) as
                                <*> f Nothing u
                                <*> f Nothing e
  relabel f (VaArg al t)          = VaArg
                                <$> T.mapM (relabel f) al
                                <*> pure t
  relabel f (ExtractElt v i)      = ExtractElt
                                <$> T.mapM (relabel f) v
                                <*> relabel f i
  relabel f (InsertElt v e i)     = InsertElt
                                <$> T.mapM (relabel f) v
                                <*> T.mapM (relabel f) e
                                <*> relabel f i
  relabel f (IndirectBr d ls)     = IndirectBr
                                <$> T.mapM (relabel f) d
                                <*> T.mapM (f Nothing) ls
  relabel f (Switch c d ls)       =
    let step (n,i) = (\l -> (n,l)) <$> f Nothing i
     in Switch <$> T.mapM (relabel f) c <*> f Nothing d <*> T.mapM step ls
  relabel f (Phi t ls)            =
    let step (a,l) = (,) <$> relabel f a <*> f Nothing l
     in Phi t <$> T.mapM step ls

  relabel f (LandingPad ty fn c cs) = LandingPad ty
                                  <$> T.mapM (relabel f) fn
                                  <*> pure c
                                  <*> T.mapM (relabel f) cs

  relabel f (Resume tv)           = Resume <$> T.mapM (relabel f) tv

instance HasLabel Clause' where
  relabel f clause = case clause of
    Catch  tv -> Catch  <$> T.mapM (relabel f) tv
    Filter tv -> Filter <$> T.mapM (relabel f) tv

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
  relabel f (ValArray t es)      = ValArray t <$> T.mapM (relabel f) es
  relabel f (ValVector pt es)    = ValVector pt <$> T.mapM (relabel f) es
  relabel f (ValStruct fs)       = ValStruct <$> T.mapM (T.mapM (relabel f)) fs
  relabel f (ValConstExpr ce)    = ValConstExpr <$> relabel f ce
  relabel f (ValLabel lab)       = ValLabel <$> f Nothing lab
  relabel f (ValPackedStruct es) =
    ValPackedStruct <$> T.mapM (T.mapM (relabel f)) es

instance HasLabel ValMd' where
  relabel f md = case md of
    ValMdString str -> pure (ValMdString str)
    ValMdValue tv   -> ValMdValue <$> T.mapM (relabel f) tv
    ValMdRef i      -> pure (ValMdRef i)
    ValMdNode es    -> ValMdNode <$> T.mapM (T.mapM (relabel f)) es
    ValMdLoc dl     -> ValMdLoc <$> relabel f dl

instance HasLabel DebugLoc' where
  relabel f dl = upd <$> relabel f (dlScope dl)
                     <*> T.mapM (relabel f) (dlIA dl)
    where
    upd scope ia = dl
      { dlScope = scope
      , dlIA    = ia
      }

instance HasLabel ConstExpr' where
  relabel f (ConstGEP inb is)   = ConstGEP inb
                              <$> T.mapM (T.mapM (relabel f)) is
  relabel f (ConstConv op a t)  = ConstConv op
                              <$> T.mapM (relabel f) a
                              <*> pure t
  relabel f (ConstSelect c l r) = ConstSelect
                              <$> T.mapM (relabel f) c
                              <*> T.mapM (relabel f) l
                              <*> T.mapM (relabel f) r
  relabel f (ConstBlockAddr t l)= ConstBlockAddr t
                              <$> f (Just t) l
