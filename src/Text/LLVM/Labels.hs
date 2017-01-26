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
    ValMdString str    -> pure (ValMdString str)
    ValMdValue tv      -> ValMdValue <$> traverse (relabel f) tv
    ValMdRef i         -> pure (ValMdRef i)
    ValMdNode es       -> ValMdNode <$> traverse (traverse (relabel f)) es
    ValMdLoc dl        -> ValMdLoc <$> relabel f dl
    ValMdDebugInfo di  -> ValMdDebugInfo <$> relabel f di

instance HasLabel DebugLoc' where
  relabel f dl = upd <$> relabel f (dlScope dl)
                     <*> traverse (relabel f) (dlIA dl)
    where
    upd scope ia = dl
      { dlScope = scope
      , dlIA    = ia
      }

instance HasLabel DebugInfo' where
  relabel f di = case di of
    DebugInfoBasicType dibt ->
      pure (DebugInfoBasicType dibt)
    DebugInfoCompileUnit dicu ->
      DebugInfoCompileUnit <$> relabel f dicu
    DebugInfoCompositeType dict ->
      DebugInfoCompositeType <$> relabel f dict
    DebugInfoDerivedType didt ->
      DebugInfoDerivedType <$> relabel f didt
    DebugInfoExpression die ->
      pure (DebugInfoExpression die)
    DebugInfoFile dif ->
      pure (DebugInfoFile dif)
    DebugInfoGlobalVariable digv ->
      DebugInfoGlobalVariable <$> relabel f digv
    DebugInfoLexicalBlock dilb ->
      DebugInfoLexicalBlock <$> relabel f dilb
    DebugInfoLexicalBlockFile dilbf ->
      DebugInfoLexicalBlockFile <$> relabel f dilbf
    DebugInfoLocalVariable dilv ->
      DebugInfoLocalVariable <$> relabel f dilv
    DebugInfoSubprogram disp ->
      DebugInfoSubprogram <$> relabel f disp
    DebugInfoSubrange disr ->
      pure (DebugInfoSubrange disr)
    DebugInfoSubroutineType dist ->
      DebugInfoSubroutineType <$> relabel f dist

instance HasLabel DIDerivedType' where
  relabel f didt = DIDerivedType
    <$> pure (didtTag didt)
    <*> pure (didtName didt)
    <*> traverse (relabel f) (didtFile didt)
    <*> pure (didtLine didt)
    <*> traverse (relabel f) (didtScope didt)
    <*> traverse (relabel f) (didtBaseType didt)
    <*> pure (didtSize didt)
    <*> pure (didtAlign didt)
    <*> pure (didtOffset didt)
    <*> pure (didtFlags didt)
    <*> traverse (relabel f) (didtExtraData didt)

instance HasLabel DISubroutineType' where
  relabel f dist = DISubroutineType
    <$> pure (distFlags dist)
    <*> traverse (relabel f) (distTypeArray dist)

instance HasLabel DIGlobalVariable' where
  relabel f digv = DIGlobalVariable
    <$> traverse (relabel f) (digvScope digv)
    <*> pure (digvName digv)
    <*> pure (digvLinkageName digv)
    <*> traverse (relabel f) (digvFile digv)
    <*> pure (digvLine digv)
    <*> traverse (relabel f) (digvType digv)
    <*> pure (digvIsLocal digv)
    <*> pure (digvIsDefinition digv)
    <*> traverse (relabel f) (digvVariable digv)
    <*> traverse (relabel f) (digvDeclaration digv)

instance HasLabel DILocalVariable' where
  relabel f dilv = DILocalVariable
    <$> traverse (relabel f) (dilvScope dilv)
    <*> pure (dilvName dilv)
    <*> traverse (relabel f) (dilvFile dilv)
    <*> pure (dilvLine dilv)
    <*> traverse (relabel f) (dilvType dilv)
    <*> pure (dilvArg dilv)
    <*> pure (dilvFlags dilv)

instance HasLabel DISubprogram' where
  relabel f disp = DISubprogram
    <$> traverse (relabel f) (dispScope disp)
    <*> pure (dispName disp)
    <*> pure (dispLinkageName disp)
    <*> traverse (relabel f) (dispFile disp)
    <*> pure (dispLine disp)
    <*> traverse (relabel f) (dispType disp)
    <*> pure (dispIsLocal disp)
    <*> pure (dispIsDefinition disp)
    <*> pure (dispScopeLine disp)
    <*> traverse (relabel f) (dispContainingType disp)
    <*> pure (dispVirtuality disp)
    <*> pure (dispVirtualIndex disp)
    <*> pure (dispFlags disp)
    <*> pure (dispIsOptimized disp)
    <*> traverse (relabel f) (dispTemplateParams disp)
    <*> traverse (relabel f) (dispDeclaration disp)
    <*> traverse (relabel f) (dispVariables disp)

instance HasLabel DICompositeType' where
  relabel f dict = DICompositeType
    <$> pure (dictTag dict)
    <*> pure (dictName dict)
    <*> traverse (relabel f) (dictFile dict)
    <*> pure (dictLine dict)
    <*> traverse (relabel f) (dictScope dict)
    <*> traverse (relabel f) (dictBaseType dict)
    <*> pure (dictSize dict)
    <*> pure (dictAlign dict)
    <*> pure (dictOffset dict)
    <*> pure (dictFlags dict)
    <*> traverse (relabel f) (dictElements dict)
    <*> pure (dictRuntimeLang dict)
    <*> traverse (relabel f) (dictVTableHolder dict)
    <*> traverse (relabel f) (dictTemplateParams dict)
    <*> pure (dictIdentifier dict)

instance HasLabel DILexicalBlock' where
  relabel f dilb = DILexicalBlock
    <$> traverse (relabel f) (dilbScope dilb)
    <*> traverse (relabel f) (dilbFile dilb)
    <*> pure (dilbLine dilb)
    <*> pure (dilbColumn dilb)

instance HasLabel DICompileUnit' where
  relabel f dicu = DICompileUnit
    <$> pure (dicuLanguage dicu)
    <*> traverse (relabel f) (dicuFile dicu)
    <*> pure (dicuProducer dicu)
    <*> pure (dicuIsOptimized dicu)
    <*> pure (dicuFlags dicu)
    <*> pure (dicuRuntimeVersion dicu)
    <*> pure (dicuSplitDebugFilename dicu)
    <*> pure (dicuEmissionKind dicu)
    <*> traverse (relabel f) (dicuEnums dicu)
    <*> traverse (relabel f) (dicuRetainedTypes dicu)
    <*> traverse (relabel f) (dicuSubprograms dicu)
    <*> traverse (relabel f) (dicuGlobals dicu)
    <*> traverse (relabel f) (dicuImports dicu)
    <*> traverse (relabel f) (dicuMacros dicu)
    <*> pure (dicuDWOId dicu)

instance HasLabel DILexicalBlockFile' where
  relabel f dilbf = DILexicalBlockFile
    <$> relabel f (dilbfScope dilbf)
    <*> traverse (relabel f) (dilbfFile dilbf)
    <*> pure (dilbfDiscriminator dilbf)

instance HasLabel ConstExpr' where
  relabel f (ConstGEP inb mp is) = ConstGEP inb
                               <$> pure mp
                               <*> traverse (traverse (relabel f)) is
  relabel f (ConstConv op a t)   = ConstConv op
                               <$> traverse (relabel f) a
                               <*> pure t
  relabel f (ConstSelect c l r)  = ConstSelect
                               <$> traverse (relabel f) c
                               <*> traverse (relabel f) l
                               <*> traverse (relabel f) r
  relabel f (ConstBlockAddr t l) = ConstBlockAddr t
                               <$> f (Just t) l
  relabel f (ConstFCmp op l r)   = ConstFCmp op
                               <$> traverse (relabel f) l
                               <*> traverse (relabel f) r
  relabel f (ConstICmp op l r)   = ConstICmp op
                               <$> traverse (relabel f) l
                               <*> traverse (relabel f) r
