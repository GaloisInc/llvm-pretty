{- |
Module      : Text.LLVM.Triple.Parse
Description : Parsing of LLVM target triples.
License     : BSD3
Maintainer  : Langston Barrett
Stability   : experimental

The declarations appear in this module in the same order as in the LLVM source.
-}

{- Note [Implementation]

The very simplest parsing functions are implemented with the 'LookupTable'
structure. For anything more complex, we endeavor to closely mirror the
structure of LLVM's implementation. This will make the code more maintainable
when updating to newer versions of LLVM.

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Text.LLVM.Triple.Parse
  ( parseArch
  , parseVendor
  , parseOS
  , parseEnv
  , parseObjFmt
  , parseSubArch
  ) where

import qualified Data.List as List

import Text.LLVM.Triple.AST
import qualified Text.LLVM.Triple.Print as Print
import Text.LLVM.Triple.Parse.LookupTable
import qualified Text.LLVM.Triple.Parse.ARM as ARM

-- | @llvm::parseArch@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/Triple.cpp#L442
parseArch :: String -> Arch
parseArch s =
  let mArch =
        -- See Note [Implementation] for the reasoning behind the strange structure.
        --
        -- It would be easy to forget to add patterns here when adding a new
        -- constructor to Arch, but we have exhaustive print-then-parse
        -- roundtrip tests to mitigate this risk.
        if | cases ["i386", "i486", "i586", "i686"] -> X86
           | cases ["i786", "i886", "i986"] -> X86
           | cases ["amd64", "x86_64", "x86_64h"] -> X86_64
           | cases ["powerpc", "powerpcspe", "ppc", "ppc32"] -> PPC
           | cases ["powerpcle", "ppcle", "ppc32le"] -> PPCLE
           | cases ["powerpc64", "ppu", "ppc64"] -> PPC64
           | cases ["powerpc64le", "ppc64le"] -> PPC64LE
           | cases ["xscale"] -> ARM
           | cases ["xscaleeb"] -> ARMEB
           | cases ["aarch64"] -> AArch64
           | cases ["aarch64_be"] -> AArch64_BE
           | cases ["aarch64_32"] -> AArch64_32
           | cases ["arc"] -> ARC
           | cases ["arm64"] -> AArch64
           | cases ["arm64_32"] -> AArch64_32
           | cases ["arm64e"] -> AArch64
           | cases ["arm"] -> ARM
           | cases ["armeb"] -> ARMEB
           | cases ["thumb"] -> Thumb
           | cases ["thumbeb"] -> ThumbEB
           | cases ["avr"] -> AVR
           | cases ["m68k"] -> M68k
           | cases ["msp430"] -> MSP430
           | cases ["mips", "mipseb", "mipsallegrex", "mipsisa32r6"
                   , "mipsr6"] -> MIPS
           | cases ["mipsel", "mipsallegrexel", "mipsisa32r6el", "mipsr6el"] -> MIPSEL
           | cases ["mips64", "mips64eb", "mipsn32", "mipsisa64r6"
                   , "mips64r6", "mipsn32r6"] -> MIPS64
           | cases ["mips64el", "mipsn32el", "mipsisa64r6el", "mips64r6el"
                   , "mipsn32r6el"] -> MIPS64EL
           | cases ["r600"] -> R600
           | cases ["amdgcn"] -> AMDGCN
           | cases ["riscv32"] -> RISCV32
           | cases ["riscv64"] -> RISCV64
           | cases ["hexagon"] -> Hexagon
           | cases ["s390x", "systemz"] -> SystemZ
           | cases ["sparc"] -> Sparc
           | cases ["sparcel"] -> SparcEL
           | cases ["sparcv9", "sparc64"] -> Sparcv9
           | cases ["tce"] -> TCE
           | cases ["tcele"] -> TCELE
           | cases ["xcore"] -> XCore
           | cases ["nvptx"] -> NVPTX
           | cases ["nvptx64"] -> NVPTX64
           | cases ["le32"] -> Le32
           | cases ["le64"] -> Le64
           | cases ["amdil"] -> AMDIL
           | cases ["amdil64"] -> AMDIL64
           | cases ["hsail"] -> HSAIL
           | cases ["hsail64"] -> HSAIL64
           | cases ["spir"] -> SPIR
           | cases ["spir64"] -> SPIR64
           | cases ["spirv32", "spirv32v1.0", "spirv32v1.1", "spirv32v1.2"
                   , "spirv32v1.3", "spirv32v1.4", "spirv32v1.5"] -> SPIRV32
           | cases ["spirv64", "spirv64v1.0", "spirv64v1.1", "spirv64v1.2"
                   , "spirv64v1.3", "spirv64v1.4", "spirv64v1.5"] -> SPIRV64
           | archPfx Kalimba -> Kalimba
           | cases ["lanai"] -> Lanai
           | cases ["renderscript32"] -> RenderScript32
           | cases ["renderscript64"] -> RenderScript64
           | cases ["shave"] -> SHAVE
           | cases ["ve"] -> VE
           | cases ["wasm32"] -> Wasm32
           | cases ["wasm64"] -> Wasm64
           | cases ["csky"] -> CSKY
           | cases ["loongarch32"] -> LoongArch32
           | cases ["loongarch64"] -> LoongArch64
           | cases ["dxil"] -> DXIL
           | otherwise -> UnknownArch
  in case mArch of
        UnknownArch ->
          if | archPfx ARM || archPfx Thumb || archPfx AArch64 ->
                 ARM.parseARMArch (ARM.ArchName s)
             | "bpf" `List.isPrefixOf` s -> parseBPFArch s
             | otherwise -> UnknownArch
        arch -> arch
  where
    cases = any (== s)
    archPfx arch = Print.archName arch `List.isPrefixOf` s

    -- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/Triple.cpp#L292
    parseBPFArch arch =
      if arch == "bpf"
      -- The way that LLVM parses the arch for BPF depends on the endianness of
      -- the host in this case, which feels deeply wrong. We don't do that, not
      -- least since we're not in IO. We default to little-endian instead.
      then BPFEL
      else if | arch == "bpf_be" || arch == "bpfeb" -> BPFEB
              | arch == "bpf_le" || arch == "bpfel" -> BPFEL
              | otherwise -> UnknownArch

-- | @llvm::parseVendor@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/Triple.cpp#L529
parseVendor :: String -> Vendor
parseVendor = lookupWithDefault table UnknownVendor
  where table = enumTable Print.vendorName

-- | @llvm::parseOS@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/Triple.cpp#L549
parseOS :: String -> OS
parseOS = lookupByPrefixWithDefault table UnknownOS
  where table = enumTable Print.osName

-- | @llvm::parseEnvironment@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/Triple.cpp#L593
parseEnv :: String -> Environment
parseEnv = lookupByPrefixWithDefault table UnknownEnvironment
  where table = enumTable Print.envName

-- | @llvm::parseFormat@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/Triple.cpp#L634
parseObjFmt :: String -> ObjectFormat
parseObjFmt = lookupBySuffixWithDefault table UnknownObjectFormat
  where table = enumTable Print.objFmtName

-- | @llvm::parseSubArch@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/Triple.cpp#L648
parseSubArch :: String -> SubArch
parseSubArch subArchName =
  if | startsWith "mips" && (endsWith "r6el" || endsWith "r6") -> MipsSubArch_r6

     | subArchName == "powerpcspe" -> PPCSubArch_spe

     | subArchName == "arm64e" -> AArch64SubArch_arm64e

     | startsWith "arm64e" -> AArch64SubArch_arm64e

     | startsWith "spirv" ->
         if | endsWith "v1.0" -> SPIRVSubArch_v10
            | endsWith "v1.1" -> SPIRVSubArch_v11
            | endsWith "v1.2" -> SPIRVSubArch_v12
            | endsWith "v1.3" -> SPIRVSubArch_v13
            | endsWith "v1.4" -> SPIRVSubArch_v14
            | endsWith "v1.5" -> SPIRVSubArch_v15
            | otherwise -> NoSubArch
     | otherwise ->
       -- TODO(lb): Big ARM case goes here
       NoSubArch
  where
    startsWith = (`List.isPrefixOf` subArchName)
    endsWith = (`List.isSuffixOf` subArchName)
