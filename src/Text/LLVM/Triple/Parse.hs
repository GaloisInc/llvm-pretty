{- |
Module      : Text.LLVM.Triple.Parse
Description : Parsing of LLVM target triples.
License     : BSD3
Maintainer  : Langston Barrett
Stability   : experimental
-}

module Text.LLVM.Triple.Parse
  ( parseVendor
  , parseOS
  ) where

import qualified Data.Maybe as Maybe
import qualified Data.List as List

import Text.LLVM.Triple.AST
import qualified Text.LLVM.Triple.Print as Print

-- | Helper, not exported
enumTable :: Bounded a => Enum a => (a -> String) -> [(String, a)]
enumTable prnt = [(prnt a, a) | a <- enumFrom minBound]

-- | Helper, not exported.
--
-- @lookupBy (==) == lookup@.
lookupBy :: (a -> Bool) -> [(a, b)] -> Maybe b
lookupBy p = Maybe.listToMaybe . map snd . filter (p . fst)

-- | @llvm::parseVendor@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/Triple.cpp#L529
parseVendor :: String -> Vendor
parseVendor s = Maybe.fromMaybe UnknownVendor (lookup s table)
  where table = enumTable Print.vendorName

-- | @llvm::parseOS@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/Triple.cpp#L549
parseOS :: String -> OS
parseOS s = Maybe.fromMaybe UnknownOS (lookupBy (`List.isPrefixOf` s) table)
  where table = enumTable Print.osName
