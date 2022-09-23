{- |
Module      : Text.LLVM.Triple.Parse.ARM
Description : ARM utilities used in target triple parsing.
License     : BSD3
Maintainer  : Langston Barrett
Stability   : experimental

This module is not exposed as part of the library API.
-}

{-# LANGUAGE FlexibleContexts #-}
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
  , getCanonicalArchName
  , parseARMArch
  ) where

import qualified Data.Char as Char
import           Control.Monad (liftM2, when)
import qualified MonadLib as M
import qualified MonadLib.Monads as M
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

data CanonicalArchNameState
  = CanonicalArchNameState
    { offset :: Int
    , archStr :: String -- ^ @A@ in the LLVM
    }

-- | @llvm::ARM::getCanonicalArchName@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/ARMTargetParser.cpp#L295
getCanonicalArchName :: ArchName -> Maybe ArchName
getCanonicalArchName (ArchName arch) =
  -- See Note [Implementation] for the reasoning behind the strange structure.
  --
  -- Could probably be translated even more directly using ContT, but that feels
  -- like a bit much.
  execState (CanonicalArchNameState 0 arch) $ do
    ifM (liftM2 (&&) (startsWith "aarch64") (contains "eb")) (return Nothing) $ do
      whenM (startsWith "arm64_32") $
        setOffset 8
      whenM (startsWith "arm64e") $
        setOffset 6
      whenM (startsWith "arm64") $
        setOffset 5
      whenM (startsWith "aarch64_32") $
        setOffset 10
      whenM (startsWith "arm") $
        setOffset 3
      whenM (startsWith "thumb") $
        setOffset 5
      whenM (startsWith "aarch64") $ do
        setOffset 7
        whenM ((== "_be") <$> archOffSubstr 3) $
          addOffset 3

      off <- offset <$> M.get
      sub <- archOffSubstr 2
      when (off /= npos && sub == "eb") $
        addOffset 2
      whenM (endsWith "eb") $ do
        changeArch (\arch' -> substr 0 (length arch' - 2) arch')
      off' <- offset <$> M.get
      when (off' /= npos) $
        changeArch (drop off')

      arch' <- archStr <$> M.get
      if | length arch' == 0 -> return Nothing
         | off' /= npos && length arch' > 2 &&
             (take 1 arch' /= "v" || not (all Char.isDigit (substr 1 1 arch'))) ->
               return Nothing
         | off' /= npos && "eb" `List.isInfixOf` arch' -> return Nothing
         | otherwise -> return (Just (ArchName arch'))
  where
    npos = 0

    ifM b thn els = b >>= \b' -> if b' then thn else els
    whenM b k = ifM b k (return ())
    startsWith pfx = (pfx `List.isPrefixOf`) . archStr <$> M.get
    endsWith sfx = (sfx `List.isSuffixOf`) . archStr <$> M.get
    contains ifx = (ifx `List.isInfixOf`) . archStr <$> M.get

    substr start sz = take sz . drop start
    archSubstr begin sz = substr begin sz . archStr <$> M.get
    archOffSubstr sz = do
      off <- offset <$> M.get
      archSubstr off sz

    changeOffset f = do
      s <- M.get
      M.set (s { offset = f (offset s) })
    addOffset n = changeOffset (n+)
    setOffset n = changeOffset (const n)

    changeArch f = M.set . (\s -> s { archStr = f (archStr s) }) =<< M.get

    -- Not in MonadLib...
    execState s = fst . M.runState s

-- | @llvm::parseARMArch@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/Triple.cpp#L377
parseARMArch :: ArchName -> Arch
parseARMArch archName =
  let
    isa = parseISAKind archName
    endian = parseEndianKind archName
    arch =
      case endian of
        Just Little ->
          case isa of
            Just ISAArm -> ARM
            Just ISAThumb -> Thumb
            Just ISAAArch64 -> AArch64
            Nothing -> UnknownArch
        Just Big ->
          case isa of
            Just ISAArm -> ARMEB
            Just ISAThumb -> ThumbEB
            Just ISAAArch64 -> AArch64_BE
            Nothing -> UnknownArch
        Nothing -> UnknownArch
    mArchName = getCanonicalArchName archName
  in case mArchName of
      Nothing -> UnknownArch
      Just (ArchName archNm) ->
        if | isa == Just ISAThumb &&
             ("v2" `List.isPrefixOf` archNm || "v3" `List.isPrefixOf` archNm) -> UnknownArch
            -- TODO(lb): LLVM has one more check here involving the "arch
            -- profile" that's not yet implemented here... Probably not a big
            -- deal because this case is only executed when the target arch
            -- doesn't match the "canonical" versions like "arm", "thumb", and
            -- "aarch64".
           | otherwise -> arch
