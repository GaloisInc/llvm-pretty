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
  { roLocation   :: [Location]
  , roRetType    :: Maybe Type
  , roEntryLabel :: Maybe Ident
  } deriving Show

emptyRO :: RO
emptyRO  = RO
  { roLocation   = []
  , roRetType    = Nothing
  , roEntryLabel = Nothing
  }

enter :: Location -> Lint a -> Lint a
enter l body = Lint $ do
  ro <- ask
  local (ro { roLocation = l : roLocation ro }) (unLint body)

withRetType :: Type -> Lint a -> Lint a
withRetType ty body = Lint $ do
  ro <- ask
  local (ro { roRetType = Just ty }) (unLint body)

withEntryLabel :: Ident -> Lint a -> Lint a
withEntryLabel l body = Lint $ do
  ro <- ask
  local (ro { roEntryLabel = Just l }) (unLint body)

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
checkDefine d = enter (Function (defName d)) body
  where
  body =
    case defBody d of
      []      -> err "No basic blocks in function body"
      entry:_ -> do
        checkEntryBasicBlock (head (defBody d))
        maybe id withEntryLabel (bbLabel entry)
            (mapM_ checkBasicBlock (defBody d))

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

checkJumpDest :: Ident -> Lint ()
checkJumpDest l = do
  ro <- Lint ask
  case roEntryLabel ro of
    Just l' | l == l' -> err "Branches to the entry block are not allowed"
    _                 -> return ()

checkEntryBasicBlock :: BasicBlock -> Lint ()
checkEntryBasicBlock bb = enter (Block (bbLabel bb)) $ do
  when (any (isPhi . stmtInstr) (bbStmts bb))
      (err "Entry basic block cannot have phi instructions")

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
    RetVoid      -> typeOfRetVoid
    Ret arg      -> typeOfRet arg
    Arith ty l r -> typeOfArith ty l r
    Jump l       -> typeOfJump l
    Br c t f     -> typeOfBr c t f
    _            -> do
      warn "not implemented"
      return Nothing

typeOfArith :: ArithOp -> Typed Value -> Value -> Lint (Maybe Type)
typeOfArith op l r | isIArith op = typeOfIArith op l r
                   | otherwise   = typeOfFArith op l r

typeOfRetVoid :: Lint (Maybe Type)
typeOfRetVoid  = do
  checkRetType (PrimType Void)
  return Nothing

-- | The ret instruction has an undefined type.
typeOfRet :: Typed Value -> Lint (Maybe Type)
typeOfRet tv = do
  checkRetType (typedType tv)
  return Nothing

-- | Integral instructions produce things of integer or vector of integer type.
typeOfIArith :: ArithOp -> Typed Value -> Value -> Lint (Maybe Type)
typeOfIArith op l r = do
  let ty = typedType l
  unless (primTypeOf ty isIntegerType || vectorElemTypeOf ty isIntegerType)
    $ err $ concat
      [ "the ``", render (ppType ty), "'' type is not valid for the ``"
      , render (ppArithOp op), "'' instruction"
      ]
  return (Just ty)

-- | Floating-point instructions produce things of float or vector of float
-- type.
typeOfFArith :: ArithOp -> Typed Value -> Value -> Lint (Maybe Type)
typeOfFArith op l r = do
  let ty = typedType l
  unless (primTypeOf ty isFloatType || vectorElemTypeOf ty isFloatType)
    $ err $ concat
      [ "the ``", render (ppType ty), "'' type is not valid for the ``"
      , render (ppArithOp op), "'' instruction"
      ]
  return (Just ty)

typeOfJump :: Ident -> Lint (Maybe Type)
typeOfJump l = do
  checkJumpDest l
  return Nothing

typeOfBr :: Typed Value -> Ident -> Ident -> Lint (Maybe Type)
typeOfBr c t f = do
  let ty = typedType c
  unless (primTypeOf ty isBooleanType)
      (err (render (ppType ty) ++ " is not valid for the br instruction"))
  checkJumpDest t
  checkJumpDest f
  return Nothing

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
typeListsAgree ts ts'
  = length ts == length ts' && and (zipWith typesAgree ts ts')

isBooleanType :: PrimType -> Bool
isBooleanType ty = ty == Integer 1

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
