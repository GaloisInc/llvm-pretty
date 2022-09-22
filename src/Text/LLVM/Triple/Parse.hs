{- |
Module      : Text.LLVM.Triple.Parse
Description : Parsing of LLVM target triples.
License     : BSD3
Maintainer  : Langston Barrett
Stability   : experimental

The declarations appear in this module in the same order as in the LLVM source.
-}

module Text.LLVM.Triple.Parse
  ( parseVendor
  , parseOS
  , parseEnv
  , parseObjFmt
  ) where

import qualified Data.Maybe as Maybe
import qualified Data.List as List

import Text.LLVM.Triple.AST
import qualified Text.LLVM.Triple.Print as Print

--------------------------------------------------------------------------------
-- LookupTable

-- | Helper, not exported
newtype LookupTable a = LookupTable { getLookupTable :: [(String, a)] }

-- | Helper, not exported
enumLookupTable :: Bounded a => Enum a => (a -> String) -> LookupTable a
enumLookupTable prnt = LookupTable [(prnt a, a) | a <- enumFrom minBound]

-- | Helper, not exported.
--
-- @lookupBy (== s) == lookup s . getLookupTable@.
lookupBy :: (String -> Bool) -> LookupTable a -> Maybe a
lookupBy p = Maybe.listToMaybe . map snd . filter (p . fst) . getLookupTable

-- | Helper, not exported.
lookupByWithDefault :: a -> (String -> Bool) -> LookupTable a -> a
lookupByWithDefault def p = Maybe.fromMaybe def . lookupBy p

-- | Helper, not exported.
lookupWithDefault :: LookupTable a -> a -> String -> a
lookupWithDefault table def val = lookupByWithDefault def (== val) table

-- | Helper, not exported.
lookupByPrefixWithDefault :: LookupTable a -> a -> String -> a
lookupByPrefixWithDefault table def pfx =
  lookupByWithDefault def (pfx `List.isPrefixOf`) table

-- | Helper, not exported.
lookupBySuffixWithDefault :: LookupTable a -> a -> String -> a
lookupBySuffixWithDefault table def sfx =
  lookupByWithDefault def (sfx `List.isSuffixOf`) table

--------------------------------------------------------------------------------
-- Parsing

-- | @llvm::parseVendor@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/Triple.cpp#L529
parseVendor :: String -> Vendor
parseVendor = lookupWithDefault table UnknownVendor
  where table = enumLookupTable Print.vendorName

-- | @llvm::parseOS@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/Triple.cpp#L549
parseOS :: String -> OS
parseOS = lookupByPrefixWithDefault table UnknownOS
  where table = enumLookupTable Print.osName

-- | @llvm::parseEnvironment@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/Triple.cpp#L593
parseEnv :: String -> Environment
parseEnv = lookupByPrefixWithDefault table UnknownEnvironment
  where table = enumLookupTable Print.envName

-- | @llvm::parseFormat@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/Triple.cpp#L634
parseObjFmt :: String -> ObjectFormat
parseObjFmt = lookupBySuffixWithDefault table UnknownObjectFormat
  where table = enumLookupTable Print.objFmtName
