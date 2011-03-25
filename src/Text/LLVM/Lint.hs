{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.LLVM.Lint where

import Text.LLVM.AST

import MonadLib
import Text.PrettyPrint.HughesPJ
import qualified Data.Map as Map


newtype Lint a = Lint
  { unLint :: ReaderT RO (WriterT [Msg] (StateT RW Id)) a
  } deriving (Functor,Monad)

runLint :: Lint a -> (a,[Msg])
runLint  = fst . runId . runStateT emptyRW . runWriterT
               . runReaderT emptyRO . unLint

type Name = Either Ident Symbol

data Location
  = Function Symbol
  | Block (Maybe Ident)
    deriving Show

data RO = RO
  { roLocation :: [Location]
  , roRetType  :: Maybe Type
  } deriving Show

emptyRO :: RO
emptyRO  = RO
  { roLocation = []
  , roRetType  = Nothing
  }

enter :: Location -> Lint a -> Lint a
enter l body = Lint $ do
  ro <- ask
  local (ro { roLocation = l : roLocation ro }) (unLint body)

withRetType :: Type -> Lint a -> Lint a
withRetType ty body = Lint $ do
  ro <- ask
  local (ro { roRetType = Just ty }) (unLint body)

data RW = RW
  { rwTypes :: Map.Map Name (Maybe Type)
  } deriving Show

emptyRW :: RW
emptyRW  = RW
  { rwTypes = Map.empty
  }

preserveRW :: Lint a -> Lint a
preserveRW body = Lint $ do
  rw  <- get
  res <- unLint body
  set rw
  return res

data Msg
  = Warn [Location] String
  | Err [Location] String
    deriving (Show)

warn :: String -> Lint ()
warn msg = Lint $ do
  ro <- ask
  put [Warn (roLocation ro) msg]

err :: String -> Lint ()
err msg = Lint $ do
  ro <- ask
  put [Err (roLocation ro) msg]


-- Type Environment ------------------------------------------------------------

addType :: Name -> Type -> Lint ()
addType n ty = Lint $ do
  rw <- get
  set rw { rwTypes = Map.insert n (Just ty) (rwTypes rw) }

undef :: Name -> Lint ()
undef n = Lint $ do
  rw <- get
  set rw { rwTypes = Map.insert n Nothing (rwTypes rw) }

addSymbol :: Symbol -> Type -> Lint ()
addSymbol  = addType . Right

addIdent :: Ident -> Type -> Lint ()
addIdent  = addType . Left

undefSymbol :: Symbol -> Lint ()
undefSymbol  = undef . Right

undefIdent :: Ident -> Lint ()
undefIdent  = undef . Left

addDeclare :: Declare -> Lint ()
addDeclare d = addSymbol (decName d) ty
  where
  ty = FunTy (decRetType d) (decArgs d)

addDefine :: Define -> Lint ()
addDefine d = addSymbol (defName d) ty
  where
  ty = FunTy (defRetType d) (map typedType (defArgs d))

-- | Add all names and types defined by this module.
addModule :: Module -> Lint ()
addModule m = do
  mapM_ addDeclare (modDeclares m)
  mapM_ addDefine  (modDefines m)


-- AST Checking ----------------------------------------------------------------

checkModule :: Module -> Lint ()
checkModule m = preserveRW $ do
  addModule m
  mapM_ checkDeclare (modDeclares m)
  mapM_ checkDefine  (modDefines m)

checkDeclare :: Declare -> Lint ()
checkDeclare d = do
  mapM_ checkInputType (decArgs d)
  checkOutputType (decRetType d)

checkDefine :: Define -> Lint ()
checkDefine d = do
  err "checkDefine not implemented"

checkInputType :: Type -> Lint ()
checkInputType ty = do
  err "checkInputType not implemented"

checkOutputType :: Type -> Lint ()
checkOutputType ty = do
  err "checkOutputType not implemented"

checkRetType :: Type -> Lint ()
checkRetType ty = do
  ro <- Lint ask
  case roRetType ro of
    Nothing  -> error "No return type set"
    Just ty'
      | typesAgree ty' ty -> return ()
      | otherwise         -> err $ concat
        [ "Return type, ``", render (ppType ty)
        , "'' does not agree with ``", render (ppType ty'), "''"
        ]

checkBasicBlock :: BasicBlock -> Lint ()
checkBasicBlock bb = enter (Block (bbLabel bb)) $ do
  let stmts = filter (not . isComment . stmtInstr) (bbStmts bb)
  unless (null stmts) $ do
    let (as,[end]) = splitAt (length stmts - 1) stmts
    forM_ as $ \s -> do
      checkStmt s
      when (isTerminator (stmtInstr s)) $ err $ concat
        [ "``", render (ppStmt s), "''"
        , " should terminate "
        , maybe "<unknown basic block>" (render . ppIdent) (bbLabel bb)
        , ", but doesn't"
        ]

    checkStmt end
    unless (isTerminator (stmtInstr end)) $ err $ concat
      [ "``", render (ppStmt end), "''"
      , " does not terminate the basic block"
      ]

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
      checkRetType (typedType ta)
      return Nothing

    TypeArg ty@(PrimType Void) -> do
      checkRetType ty
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

test = BasicBlock
  { bbLabel = Just (Ident "Loop")
  , bbStmts =
    [ Effect retVoid
    , Effect retVoid
    , Effect (comment "a")
    ]
  }
