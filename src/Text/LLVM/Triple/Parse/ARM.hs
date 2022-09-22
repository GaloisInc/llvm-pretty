{- |
Module      : Text.LLVM.Triple.Parse.ARM
Description : ARM utilities used in target triple parsing.
License     : BSD3
Maintainer  : Langston Barrett
Stability   : experimental

This module is not exposed as part of the library API.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StrictData #-}

module Text.LLVM.Triple.Parse.ARM
  ( ArchName(..)
    -- * Endianness
  , EndianKind(..)
  , parseEndianKind
    -- * ISA
  , ISAKind(..)
  , parseISAKind
    -- * Arch
  , parseARMArch
  ) where

import qualified Data.List as List

import           Text.LLVM.Triple.AST
import qualified Text.LLVM.Triple.Parse.LookupTable as Lookup

-- | The "arch" portion of a target triple
newtype ArchName = ArchName { getArchName :: String }

--------------------------------------------------------------------------------
-- Endianness

-- | @llvm::EndianKind@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/include/llvm/Support/ARMTargetParser.h#L166
data EndianKind
  = Little
  | Big
  deriving (Bounded, Enum, Eq, Ord)

-- | @llvm::ARM::parseArchEndian@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/ARMTargetParser.cpp#L248
parseEndianKind :: ArchName -> Maybe EndianKind
parseEndianKind (ArchName arch) =
  if | hasPfx "armeb" || hasPfx "thumbeb" || hasPfx "aarch64_be" -> Just Big
     | hasPfx "arm" || hasPfx "thumb" ->
         if hasSfx "eb"
         then Just Big
         else Just Little
     | hasPfx "aarch64" || hasPfx "aarch64_32" -> Just Little
     | otherwise -> Nothing
  where
    hasPfx = (`List.isPrefixOf` arch)
    hasSfx = (`List.isSuffixOf` arch)

--------------------------------------------------------------------------------
-- ISA

-- | @llvm::ISAKind@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/include/llvm/Support/ARMTargetParser.h#L162
data ISAKind
  = ISAArm
  | ISAThumb
  | ISAAArch64
  deriving (Bounded, Enum, Eq, Ord)

-- | @llvm::ARM::parseArchISA@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/ARMTargetParser.cpp#L267
parseISAKind :: ArchName -> Maybe ISAKind
parseISAKind (ArchName arch) = Lookup.lookupByPrefix arch table
  where
    table =
      Lookup.makeTable
        [ ("aarch64", ISAAArch64)
        , ("arm64", ISAAArch64)
        , ("thumb", ISAThumb)
        , ("arm", ISAArm)
        ]

--------------------------------------------------------------------------------
-- Arch

-- | @llvm::parseARMArch@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/Triple.cpp#L377
parseARMArch :: ArchName -> Maybe Arch
parseARMArch archName =
  let
    isa = parseISAKind archName
    endian = parseEndianKind archName
    arch =
      case endian of
        Just Little ->
          case isa of
            Just ISAArm -> Just ARM
            Just ISAThumb -> Just Thumb
            Just ISAAArch64 -> Just AArch64
            Nothing -> Nothing
        Just Big ->
          case isa of
            Just ISAArm -> Just ARMEB
            Just ISAThumb -> Just ThumbEB
            Just ISAAArch64 -> Just AArch64_BE
            Nothing -> Nothing
        Nothing -> Nothing
  in -- TODO(lb): LLVM does some complicated validity checks here, they're not
     -- yet implemented here... Probably not a big deal because this case is
     -- only executed when the target arch doesn't match the "canonical" versions
     -- like "arm", "thumb", and "aarch64".
     arch
