{-# LANGUAGE LambdaCase #-}

{- |
Module      : Text.LLVM.Combine
Description : Combine LLVM Modules
License     : BSD3
Maintainer  : Kevin Quick <kquick@galois.com>
Stability   : provisional

Thos module provides the ability to smash together LLVM Module specifications to
provide the ability to load separate LLVM Modules (e.g. bitcode files) and
analyze them as if they had been linked together as a single program.

-}

module Text.LLVM.Combine
  (
    llvmModuleCombine
  )
where

import Data.Bool ( bool )
import Data.Generics.Schemes ( everywhere )
import Data.Generics.Aliases ( mkT )
import Lens.Micro
import Lens.Micro.Extras
import Data.Function ( on )
import Data.List ( find )
import Data.Maybe ( fromMaybe )
import Data.String ( fromString )
import Text.LLVM.AST
import Text.LLVM.Lens


-- | Combines LLVM Modules into a single, composite Module.  This is akin to
-- linking, but just from the perspective of what is needed for program analysis.
--
-- This differs from `llvm-link` in the following known ways:
--
-- 1. The `llvm-link` tool uses structural typing resolution: if two modules
--    each have a type with the same structure, the resulting module will only
--    have one type; the name from one of the modules is chosen and all
--    references to the typename in the other module will be rewritten to the
--    first module.
--
--    The `llvmModuleCombine` takes a slightly different approach: types are not
--    structurally coalesced, but this means that type names are deconflicted by
--    adding a numbered suffix.  This still requires modifying the type name
--    throughout that module, but (a) there are probably fewer type name
--    conflicts than structural equivalences, and (b) the original name is still
--    part of the new name which maintains origin information.
--
-- 2. The `llvm-link` tool will occasionally rewrite calls to llvm intrinsics to
--    explicitly add the default personality specification.  For example,
--    `llvm.stacksave` may be rewritten to `llvm.stacksave.p0`.  Because these
--    are intrinsics, this should not have any significant impact on the result,
--    but `llvmModuleCombine` does not perform this naming update.
--
-- 3. External declaration resolution is type independent and only name
--    sensitive.  If Module A has an external declaration `declare @f(i32 x)` and
--    Module B has a definition `define @f(float x)`, this `llvmModuleCombine`
--    operation will use the latter to satisfy the former (by removing the
--    former) even though the types do not match.
--
llvmModuleCombine :: Module -> Module -> Module
llvmModuleCombine a addModule =
  let defs = a ^. modDefinesLens
      decls = a ^. modDeclaresLens
      newDefs = b ^. modDefinesLens
      newDecls = b ^. modDeclaresLens
      rmvDefined = flip (foldr removeDefined)
      newDeclsLessOldDefs = rmvDefined defs newDecls
      oldDeclsLessNewDefs = rmvDefined newDefs decls
      joinedName n = Just $ fromMaybe "..." n <> "+" <> fromMaybe "..." (modSourceName b)
      newUmdBase = let umIdxs = umIndex <$> modUnnamedMd a
                   in bool (succ $ maximum umIdxs) (UnnamedMdIdx 0) $ null umIdxs
      -- unnamed metadata is referenced almost everywhere, so update that globally
      -- first:
      b = updateUmd newUmdBase (deConflictTypes addModule (a ^. modTypesLens))
  in a
     & modSourceNameLens %~ joinedName
     & modDeclaresLens .~ (oldDeclsLessNewDefs <> newDeclsLessOldDefs)
     & modDefinesLens %~ deConflict (b ^. modDefinesLens)
     & modTypesLens <>~ b ^. modTypesLens
     & modUnnamedMdLens <>~ b ^. modUnnamedMdLens
     & modNamedMdLens <>~ b ^. modNamedMdLens
     & modComdatLens <>~ b ^. modComdatLens
     & modGlobalsLens <>~ b ^. modGlobalsLens
     & modInlineAsmLens <>~ b ^. modInlineAsmLens
     & modAliasesLens <>~ b ^. modAliasesLens
  -- TODO Globals any should override Linkage external for the same name
  -- TODO verify modTriple and modDataLayout are the same?


-- | Rewrites type references in the input module to ensure uniqueness against
-- all types mentioned in the second module.
deConflictTypes :: Module -> [TypeDecl] -> Module
deConflictTypes inpMod existingTypes =
  let resolveTypeConflict m t =
        if any (((==) `on` typeName) t) existingTypes
        then renameType m t (let Ident n = typeName t in n <> "___") (0 :: Int)
        else m
      renameType m t b n =
        let newName = Ident (b <> show n)
        in if any ((newName ==) . typeName) existingTypes
           then if n > 100000
                then error $ "Unable to generate unique type name for " <> b
                else renameType m t b $ succ n
           else everywhere (mkT (chngType (typeName t) newName)) m
      chngType oldName newName n = bool n newName $ n == oldName
  in foldl resolveTypeConflict inpMod (inpMod ^. modTypesLens)


-- | A Define takes precedence over a Declare.  When combining modules, module A
-- may Declare a function that is handled by a Define in module B, so get rid of
-- the Declare when putting A and B together.

removeDefined :: Define -> [Declare] -> [Declare]
removeDefined def = filter ((def ^. defNameLens /=) . view decNameLens)


-- | Module A and Module B may have a Define with the same name (Symbol).  This
-- is normal when linking multiple modules together, and is resolved by linkers
-- as guided by the Linkage information for the two Definitions, usually by
-- either renaming or merging.

deConflict :: [Define] -> [Define] -> [Define]
deConflict new curr = uncurry (<>) $ foldl deConflictDef (curr, new) new
  where
    deConflictDef (ads, bds) bd =
      case find (((==) `on` defName) bd) ads of
        Nothing -> (ads, bds)
        Just ad -> handle ads bds ad bd
    handle ads bds ad bd =
      case bd ^. defLinkageLens of
        Just Private -> (ads, renameDef bd (view defNameLens <$> ads) bds)
        Just LinkerPrivate -> (ads, renameDef bd (view defNameLens <$> ads) bds)
        Just LinkerPrivateWeak ->
          (ads, renameDef bd (view defNameLens <$> ads) bds) -- ??
        Just LinkerPrivateWeakDefAuto ->
          (ads, renameDef bd (view defNameLens <$> ads) bds) -- ??
        Just Internal -> (ads, renameDef bd (view defNameLens <$> ads) bds)
        Just AvailableExternally ->
          -- Never happen: not allowed on defines.  Ignore
          (ads, bds)
        Just Linkonce -> (mergeDef ad bd ads, removeDef bd bds)
        Just Weak -> (mergeDef ad bd ads, removeDef bd bds)
        Just Common -> (mergeDef ad bd ads, removeDef bd bds)
        Just ExternWeak -> (mergeDef ad bd ads, removeDef bd bds)
        Just LinkonceODR -> (mergeDef ad bd ads, removeDef bd bds)
        Just WeakODR -> (mergeDef ad bd ads, removeDef bd bds)
        Just Appending -> (appendDef ad bd ads, removeDef bd bds)
        Just External ->
          -- This should never happen: it is truly a symbol conflict.  A
          -- linker would reject this, but here we will just preserve the
          -- original.
          (ads, removeDef bd bds)
        Just DLLImport -> (ads, removeDef bd bds) -- ??
        Just DLLExport -> (ads, removeDef bd bds) -- ??
        Nothing ->
          -- No linkage specified.  The default is 'External', with associated
          -- considerations as documented for that case above.
          (ads, removeDef bd bds)

-- Note: Used for Linkonce, Weak, Common, ExternWeak, LinkonceODR, WeakODR. LLVM
-- docs say "merged", but also indicates that maybe there is a replacement
-- instead?  For now, treat "merged" as appending.
mergeDef :: Define -> Define -> [Define] -> [Define]
mergeDef = appendDef

appendDef :: Define -> Define -> [Define] -> [Define]
appendDef d1 d2 =
  let appenD = d1 & defBodyLens <>~ d2 ^. defBodyLens
  in (appenD :) . filter (((/=) `on` defName) d1)

removeDef :: Define -> [Define] -> [Define]
removeDef d = filter (((/=) `on` defName) d)


-- Renames the Defined symbol to a new name using a discriminator to avoid a
-- conflict.  Only valid for renaming Private/Internal Defines such that changing
-- any reference to the original Symbol to the new Symbol in the provided set of
-- Defines is sufficient to change all references.  Note therefore this excludes:
-- renaming of global variables, changing a GlobalAlias.

renameDef :: Define -> [Symbol] -> [Define] -> [Define]
renameDef toRename known inDefs =
  -- KWQ TODO: needs to change GlobalAlias aliasName?
  --
  let getNewName nm n =
        -- Note: adds a "discriminator" to the name in a way that is valid for
        -- both C functions and C++ mangled names (see
        -- https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangling-scope).
        let nn = if n < 10
                 then nm <> "_" <> show n
                 else nm <> "__" <> show n <> "_"
        in case find ((fromString nn ==) . defName) inDefs of
             Just _ -> getNewName nm $ succ n
             Nothing ->
               if fromString nn `elem` known
               then getNewName nm $ succ n
               else nn
      (Symbol oldname) = defName toRename
      newName = Symbol $ getNewName oldname (1 :: Integer)
  in changeSym (defName toRename) newName inDefs


changeSym :: Symbol -> Symbol -> [Define] -> [Define]
changeSym old new = everywhere (mkT chngSym)
  where
    chngSym s = bool s new $ old == s

-- | Adjusts all unnamed metadata indices in the Module to begin at the specified
-- newBase, which allows this module to be combined without conflict with a
-- module whose metadata indices are all below the newBase.
updateUmd :: UnnamedMdIdx -> Module -> Module
updateUmd newBase = everywhere (mkT (\n -> n + newBase))
