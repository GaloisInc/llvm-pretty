{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.LLVM.Lint where

import Text.LLVM.AST

import MonadLib
import Text.PrettyPrint.HughesPJ
import qualified Data.Map as Map


newtype Lint a = Lint
  { unLint :: WriterT [Msg] (StateT RW Id) a
  } deriving (Functor,Monad)

runLint :: Lint a -> (a,[Msg])
runLint  = fst . runId . runStateT emptyRW . runWriterT . unLint

type Name = Either Ident Symbol

data RW = RW
  { rwTypes   :: Map.Map Name (Maybe Type)
  , rwFunType :: Maybe Type
  } deriving Show

data Msg
  = Warn String
  | Err String
    deriving (Show)

warn :: String -> Lint ()
warn msg = Lint (put [Warn msg])

err :: String -> Lint ()
err msg = Lint (put [Err msg])

emptyRW :: RW
emptyRW  = RW
  { rwTypes   = Map.empty
  , rwFunType = Nothing
  }

addType :: Name -> Type -> Lint ()
addType n ty = Lint $ do
  rw <- get
  set rw { rwTypes = Map.insert n (Just ty) (rwTypes rw) }

undef :: Name -> Lint ()
undef n = Lint $ do
  rw <- get
  set rw { rwTypes = Map.insert n Nothing (rwTypes rw) }

checkFunType :: Type -> Lint ()
checkFunType ty = do
  rw <- Lint get
  case rwFunType rw of
    Nothing               -> Lint (set rw { rwFunType = Just ty })
    Just ty'
      | typesAgree ty ty' -> return ()
      | otherwise         -> err $ concat
        [ "Return type, ``", render (ppType ty)
        , "'' does not agree with ``", render (ppType ty'), "''"
        ]

addSymbol :: Symbol -> Type -> Lint ()
addSymbol  = addType . Right

addIdent :: Ident -> Type -> Lint ()
addIdent  = addType . Left

undefSymbol :: Symbol -> Lint ()
undefSymbol  = undef . Right

undefIdent :: Ident -> Lint ()
undefIdent  = undef . Left

addDefine :: Define -> Lint ()
addDefine d = addSymbol (defName d) ty
  where
  ty = FunTy (defRetType d) (map typedType (defArgs d))

checkBasicBlock :: BasicBlock -> Lint ()
checkBasicBlock bb = mapM_ checkStmt (bbStmts bb)

checkStmt :: Stmt -> Lint ()
checkStmt stmt =
  case stmt of
    Effect i     -> checkInstr i
    Result var i -> do
      mb <- typeOfInstr i
      case mb of
        Nothing -> undefIdent var
        Just ty -> addIdent var ty

checkInstr :: Instr -> Lint ()
checkInstr instr = do
  _ <- typeOfInstr instr
  return ()

typeOfInstr :: Instr -> Lint (Maybe Type)
typeOfInstr instr =
  case instr of
    Ret arg  -> typeOfRet arg
    Add l r  -> typeOfAdd l r
    FAdd l r -> typeOfFAdd l r
    _        -> do
      warn "not implemented"
      return Nothing

-- | The ret instruction has an undefined type.
typeOfRet :: Arg -> Lint (Maybe Type)
typeOfRet arg =
  case arg of
    TypedArg ta -> do
      checkFunType (typedType ta)
      return Nothing

    TypeArg ty@(PrimType Void) -> do
      checkFunType ty
      return Nothing

    TypeArg _ -> do
      err ("Invalid type argument to ret: " ++ render (ppArg arg))
      return Nothing

    UntypedArg _ -> do
      err ("Invalid untyped argument to ret: " ++ render (ppArg arg))
      return Nothing

-- | add produces things of integer or vector of integer type.
typeOfAdd :: Typed Value -> Value -> Lint (Maybe Type)
typeOfAdd l r = do
  let ty = typedType l
  unless (primTypeOf ty isIntegerType || vectorElemTypeOf ty isIntegerType)
    (err (render (ppType ty) ++ " is not valid for the add instruction "))
  return (Just (typedType l))

-- | fadd produces things of float or vector of float type.
typeOfFAdd :: Typed Value -> Value -> Lint (Maybe Type)
typeOfFAdd l r = do
  let ty = typedType l
  unless (primTypeOf ty isFloatType || vectorElemTypeOf ty isFloatType)
    (err (render (ppType ty) ++ " is not valid for the fadd instruction "))
  return (Just (typedType l))

-- Types -----------------------------------------------------------------------

typesAgree :: Type -> Type -> Bool
typesAgree (PrimType p)     (PrimType p')     = p == p'
typesAgree (Alias i)        (Alias i')        = i == i'
typesAgree (Array l t)      (Array l' t')     = l == l' && typesAgree t t'
typesAgree (PtrTo t)        (PtrTo t')        = typesAgree t t'
typesAgree (Struct t)       (Struct t')       = typeListsAgree t t'
typesAgree (Vector l p)     (Vector l' p')    = l == l' && p == p'
typesAgree (PackedStruct t) (PackedStruct t') = typeListsAgree t t'
typesAgree (FunTy r as)     (FunTy r' as')    = typesAgree r r'
                                             && typeListsAgree as as'

typeListsAgree :: [Type] -> [Type] -> Bool
typeListsAgree ts ts' = and (zipWith typesAgree ts ts')

isIntegerType :: PrimType -> Bool
isIntegerType Integer{} = True
isIntegerType _         = False

isFloatType :: PrimType -> Bool
isFloatType FloatType{} = True
isFloatType _           = False

primTypeOf :: Type -> (PrimType -> Bool) -> Bool
primTypeOf (PrimType pt) f = f pt
primTypeOf _             _ = False

vectorElemTypeOf :: Type -> (PrimType -> Bool) -> Bool
vectorElemTypeOf (Vector _ pty) f = f pty
vectorElemTypeOf _              _ = False


-- Tests -----------------------------------------------------------------------
