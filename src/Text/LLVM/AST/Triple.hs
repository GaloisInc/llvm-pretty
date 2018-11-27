{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{- |
Module      : Text.LLVM.AST.Triple
Description : AST and parsing of LLVM target triples.
License     : BSD3
Maintainer  : Langston Barrett
Stability   : experimental
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Text.LLVM.AST.Triple where

import Data.Maybe (fromMaybe)
import Data.List (foldl', elem, isPrefixOf, isSuffixOf)
import Data.Monoid (First(..))
import Data.List.Split (splitOn)

import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- import           Text.Parsec
-- import           Text.Parsec.String

-- | https://github.com/llvm-mirror/llvm/blob/release_60/include/llvm/ADT/Triple.h#L46
--
-- The constructors are consistently named across big-endian (BE) and little-endian (LE),
-- unlike their counterparts in the LLVM source.
data Arch =
    UnknownArch String -- Unknown (with the unrecognized string)
  | ARM                -- ARM (little endian): arm, armv.*, xscale
  | ARMBE              -- ARM (big endian): armeb
  | AArch64            -- AArch64 (little endian): aarch64
  | AArch64BE          -- AArch64 (big endian): aarch64_be
  | ARC                -- ARC: Synopsys ARC
  | AVR                -- AVR: Atmel AVR microcontroller
  | BPFLE              -- eBPF or extended BPF or 64-bit BPF (little endian)
  | BPFBE              -- eBPF or extended BPF or 64-bit BPF (big endian)
  | Hexagon            -- Hexagon: hexagon
  | MIPS               -- MIPS: mips, mipsallegrex
  | MIPSLE             -- MIPSEL: mipsel, mipsallegrexel
  | MIPS64             -- MIPS64: mips64
  | MIPS64LE           -- MIPS64EL: mips64el
  | MSP430             -- MSP430: msp430
  | NIOS2              -- NIOSII: nios2
  | PPC                -- PPC: powerpc
  | PPC64              -- PPC64: powerpc64, ppu
  | PPC64LE            -- PPC64LE: powerpc64le
  | R600               -- R600: AMD GPUs HD2XXX - HD6XXX
  | AMDGCN             -- AMDGCN: AMD GCN GPUs
  | RISCV32            -- RISC-V (32-bit): riscv32
  | RISCV64            -- RISC-V (64-bit): riscv64
  | SPARC              -- Sparc: sparc
  | SPARCV9            -- Sparcv9: Sparcv9
  | SPARCLE            -- Sparc: (endianness = little). NB: 'Sparcle' is a CPU variant
  | SystemZ            -- SystemZ: s390x
  | TCE                -- TCE (http://tce.cs.tut.fi/): tce
  | TCELE              -- TCE little endian (http://tce.cs.tut.fi/): tcele
  | Thumb              -- Thumb (little endian): thumb, thumbv.*
  | ThumbBE            -- Thumb (big endian): thumbeb
  | X86                -- X86: i[3-9]86
  | X8664              -- X86-64: amd64, x86_64
  | XCore              -- XCore: xcore
  | NVPTX              -- NVPTX: 32-bit
  | NVPTX64            -- NVPTX: 64-bit
  | LE32               -- le32: generic little-endian 32-bit CPU (PNaCl)
  | LE64               -- le64: generic little-endian 64-bit CPU (PNaCl)
  | AMDIL              -- AMDIL
  | AMDIL64            -- AMDIL with 64-bit pointers
  | HSAIL              -- AMD HSAIL
  | HSAIL64            -- AMD HSAIL with 64-bit pointers
  | SPIR               -- SPIR: standard portable IR for OpenCL 32-bit version
  | SPIR64             -- SPIR: standard portable IR for OpenCL 64-bit version
  | Kalimba            -- Kalimba: generic kalimba
  | SHAVE              -- SHAVE: Movidius vector VLIW processors
  | Lanai              -- Lanai: Lanai 32-bit
  | WASM32             -- WebAssembly with 32-bit pointers
  | WASM64             -- WebAssembly with 64-bit pointers
  | RenderScript32     -- 32-bit RenderScript
  | RenderScript64     -- 64-bit RenderScript
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

-- | A semigroup instance that simply drops the RHS, unless the LHS
-- is unknown and the RHS is not.
--
-- This is basically like 'First' from "Data.Monoid".
instance Semigroup Arch where
  (<>) a               (UnknownArch _) = a
  (<>) (UnknownArch _) b               = b
  (<>) a               _               = a

-- | https://github.com/llvm-mirror/llvm/blob/release_60/include/llvm/ADT/Triple.h#L101
--
-- The @NoSubArch@ case is taken care of in the canonical Haskell way: A @Maybe@
-- in @TargetTriple@.
data SubArch =
    ARMSubArchV8_3a
  | ARMSubArchV8_2a
  | ARMSubArchV8_1a
  | ARMSubArchV8
  | ARMSubArchV8r
  | ARMSubArchV8m_baseline
  | ARMSubArchV8m_mainline
  | ARMSubArchV7
  | ARMSubArchV7em
  | ARMSubArchV7m
  | ARMSubArchV7s
  | ARMSubArchV7k
  | ARMSubArchV7ve
  | ARMSubArchV6
  | ARMSubArchV6m
  | ARMSubArchV6k
  | ARMSubArchV6t2
  | ARMSubArchV5
  | ARMSubArchV5te
  | ARMSubArchV4t

  | KalimbaSubArchV3
  | KalimbaSubArchV4
  | KalimbaSubArchV5
  deriving (Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

-- | https://github.com/llvm-mirror/llvm/blob/release_60/include/llvm/ADT/Triple.h#L129
data Vendor =
  UnknownVendor String
  | Apple
  | PC
  | SCEI
  | BGP
  | BGQ
  | Freescale
  | IBM
  | ImaginationTechnologies
  | MipsTechnologies
  | NVIDIA
  | CSR
  | Myriad
  | AMD
  | Mesa
  | SUSE
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

-- | A semigroup instance like that of 'Arch'.
instance Semigroup Vendor where
  (<>) a                 (UnknownVendor _) = a
  (<>) (UnknownVendor _) b                 = b
  (<>) a                 _                 = a

-- | https://github.com/llvm-mirror/llvm/blob/release_60/include/llvm/ADT/Triple.h#L149
data OS =
    UnknownOS String
  | Ananas
  | CloudABI
  | Darwin
  | DragonFly
  | FreeBSD
  | Fuchsia
  | IOS
  | KFreeBSD
  | Linux
  | Lv2     -- PS3
  | MacOSX
  | NetBSD
  | OpenBSD
  | Solaris
  | Win32
  | Haiku
  | Minix
  | RTEMS
  | NaCl    -- Native Client
  | CNK     -- BG/P Compute-Node Kernel
  | AIX
  | CUDA    -- NVIDIA CUDA
  | NVCL    -- NVIDIA OpenCL
  | AMDHSA  -- AMD HSA Runtime
  | PS4
  | ELFIAMCU
  | TvOS    -- Apple tvOS
  | WatchOS -- Apple watchOS
  | Mesa3D
  | Contiki
  | AMDPAL  -- AMD PAL Runtime
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

data Environment =
    UnknownEnvironment String
  | GNU
  | GNUABIN32
  | GNUABI64
  | GNUEABI
  | GNUEABIHF
  | GNUX32
  | CODE16
  | EABI
  | EABIHF
  | Android
  | Musl
  | MuslEABI
  | MuslEABIHF
  | MSVC
  | Itanium
  | Cygnus
  | AMDOpenCL
  | CoreCLR
  | OpenCL
  | Simulator  -- Simulator variants of other systems e.g. Apple's iOS
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

-- | A semigroup instance like that of 'Arch'.
instance Semigroup Environment where
  (<>) a                      (UnknownEnvironment _) = a
  (<>) (UnknownEnvironment _) b                      = b
  (<>) a                      _                      = a

data ObjectFormat =
    UnknownObjectFormat String
  | COFF
  | ELF
  | MachO
  | Wasm
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

-- | More like a sextuple, if you think about it
type TargetTriple =
  ( Maybe Arch
  , Maybe SubArch
  , Maybe Vendor
  , Maybe OS
  , Maybe Environment
  , Maybe ObjectFormat
  )

-- | Parse a target triple string.
--
-- This does _not_ claim to be comprehensive yet: it is a best-effort.
-- In particular, the ARM arch/subarch parser is unimplemented.
--
-- It silently drops extra fields (i.e. things after extra @-@).
--
-- https://github.com/llvm-mirror/llvm/blob/release_60/lib/Support/Triple.cpp#L691
parseTargetTriple :: String -> TargetTriple
parseTargetTriple str = flattenSix $
  case splitOn "-" str of
    []             -> (Nothing, (Nothing, (Nothing, (Nothing, (Nothing, Nothing)))))
    (archStr : as) -> (\rest -> (Just (parseArch archStr), (parseSubArch archStr, rest))) $
      case as of
        []               -> (Nothing, (Nothing, (Nothing, Nothing)))
        (vendorStr : bs) -> (Just (parseVendor vendorStr),) $
          case bs of
            []           -> (Nothing, (Nothing, Nothing))
            (osStr : cs) -> (Just (parseOS osStr),) $
              case cs of
                []           -> (Nothing, Nothing)
                (envStr : _) -> ( Just (parseEnvironment envStr)
                                , Just (parseObjectFormat envStr))
  where
    -- | Flatten a nested 6-tuple into just a 6-tuple
    flattenSix :: (a, (b, (c, (d, (e, f))))) -> (a, b, c, d, e, f)
    flattenSix    (a, (b, (c, (d, (e, f))))) =  (a, b, c, d, e, f)

    -- | Parse a piece of data using a table and a comparison function
    parseFromTable :: (k -> String -> Bool) -- ^ Comparison function
                   -> [(k, a)]              -- ^ Lookup table
                   -> (String -> a)         -- ^ Function to translate "unknown" into result
                   -> String                -- ^ Input to look up
                   -> a
    parseFromTable cmp table f s =
      let foldf acc (key, val) = acc <> First (if cmp key s then Just val else Nothing)
      in fromMaybe (f s) . getFirst $
           foldl' foldf (First Nothing) table

    -- | Parse a piece of data from a "lookup table", mapping prefixes to constructors
    parseFromPrefixTable = parseFromTable isPrefixOf
    parseFromSuffixTable = parseFromTable isSuffixOf
    parseFromEqTable     = parseFromTable (==)
    parseFromElemTable   = parseFromTable (\ks s -> s `elem` ks)


    -- | https://github.com/llvm-mirror/llvm/blob/release_60/lib/Support/Triple.cpp#L381
    -- See also: https://github.com/llvm-mirror/llvm/blob/release_60/lib/Support/Triple.cpp#L259
    parseArch :: String -> Arch
    parseArch =
      let table =
            [ (["i386", "i486", "i586", "i686"]   , X86)
            , (["i786", "i886", "i986"]           , X86)
            , (["amd64", "x86_64", "x86_64h"]     , X8664)
            , (["powerpc", "ppc32"]               , PPC)
            , (["powerpc64", "ppu", "ppc64"]      , PPC64)
            , (["powerpc64le", "ppc64le"]         , PPC64LE)
            , (["xscale"]                         , ARM)
            , (["xscaleeb"]                       , ARMBE)
            , (["aarch64"]                        , AArch64)
            , (["aarch64_be"]                     , AArch64BE)
            , (["arc"]                            , ARC)
            , (["arm64"]                          , AArch64)
            , (["arm"]                            , ARM)
            , (["armeb"]                          , ARMBE)
            , (["thumb"]                          , Thumb)
            , (["thumbeb"]                        , ThumbBE)
            , (["avr"]                            , AVR)
            , (["msp430"]                         , MSP430)
            , (["mips", "mipseb", "mipsallegrex"] , MIPS)
            , (["mipsel", "mipsallegrexel"]       , MIPSLE)
            , (["mips64", "mips64eb"]             , MIPS64)
            , (["mips64el"]                       , MIPS64LE)
            , (["nios2"]                          , NIOS2)
            , (["r600"]                           , R600)
            , (["amdgcn"]                         , AMDGCN)
            , (["riscv32"]                        , RISCV32)
            , (["riscv64"]                        , RISCV64)
            , (["hexagon"]                        , Hexagon)
            , (["s390x", "systemz"]               , SystemZ)
            , (["sparc"]                          , SPARC)
            , (["sparcel"]                        , SPARCLE)
            , (["sparcv9", "sparc64"]             , SPARCV9)
            , (["tce"]                            , TCE)
            , (["tcele"]                          , TCELE)
            , (["xcore"]                          , XCore)
            , (["nvptx"]                          , NVPTX)
            , (["nvptx64"]                        , NVPTX64)
            , (["le32"]                           , LE32)
            , (["le64"]                           , LE64)
            , (["amdil"]                          , AMDIL)
            , (["amdil64"]                        , AMDIL64)
            , (["hsail"]                          , HSAIL)
            , (["hsail64"]                        , HSAIL64)
            , (["spir"]                           , SPIR)
            , (["spir64"]                         , SPIR64)
            , (["lanai"]                          , Lanai)
            , (["shave"]                          , SHAVE)
            , (["wasm32"]                         , WASM32)
            , (["wasm64"]                         , WASM64)
            , (["renderscript32"]                 , RenderScript32)
            , (["renderscript64"]                 , RenderScript64)
            ]
        in parseFromElemTable table (\s -> parseBPFArch s <> parseKalimbaArch s)
      where -- https://github.com/llvm-mirror/llvm/blob/release_60/lib/Support/Triple.cpp#L244
            -- This is done differently than in LLVM. In the case of the literal "bpf",
            -- they case on the arch of the host, which is perhaps not the best
            -- choice for a parser.
            parseBPFArch :: String -> Arch
            parseBPFArch =
              let table =
                    [ ("bpf_be" , BPFBE)
                    , ("bpf_le" , BPFLE)
                    , ("bpfbe"  , BPFBE)
                    , ("bpfle"  , BPFLE)
                    ]
              in parseFromEqTable table UnknownArch

            -- This is the only one parsed as a prefix, because of interactions
            -- with its subarches.
            -- https://github.com/llvm-mirror/llvm/blob/release_60/lib/Support/Triple.cpp#L429
            parseKalimbaArch :: String -> Arch
            parseKalimbaArch = parseFromPrefixTable [("kalimba", Kalimba)] UnknownArch

            -- TODO: ARM arch parsing

            -- https://github.com/llvm-mirror/llvm/blob/release_60/lib/Support/Triple.cpp#L316
            -- parseArchEndian :: String -> _
            -- parseArchISA :: String -> _
            -- parseARMArch :: String -> Arch
            -- parseARMArch = _

    -- https://github.com/llvm-mirror/llvm/blob/release_60/lib/Support/Triple.cpp#L542
    -- Note: this gets passed the entire arch string
    --
    -- TODO: ARM sub arch parsing
    -- https://github.com/llvm-mirror/llvm/blob/release_60/lib/Support/Triple.cpp#L554
    parseSubArch :: String -> Maybe SubArch
    parseSubArch =
      let table =
            [ ("kalimba3" , KalimbaSubArchV3)
            , ("kalimba4" , KalimbaSubArchV4)
            , ("kalimba5" , KalimbaSubArchV5)
            ]
      in parseFromSuffixTable (map (\(a, b) -> (a, Just b)) table) (const Nothing)

    -- https://github.com/llvm-mirror/llvm/blob/release_60/lib/Support/Triple.cpp#L451
    parseVendor :: String -> Vendor
    parseVendor =
      let table =
            [ ("apple"  , Apple)
            , ("pc"     , PC)
            , ("scei"   , SCEI)
            , ("bgp"    , BGP)
            , ("bgq"    , BGQ)
            , ("fsl"    , Freescale)
            , ("ibm"    , IBM)
            , ("img"    , ImaginationTechnologies)
            , ("mti"    , MipsTechnologies)
            , ("nvidia" , NVIDIA)
            , ("csr"    , CSR)
            , ("myriad" , Myriad)
            , ("amd"    , AMD)
            , ("mesa"   , Mesa)
            , ("suse"   , SUSE)
            ]
      in parseFromEqTable table UnknownVendor

    -- https://github.com/llvm-mirror/llvm/blob/release_60/lib/Support/Triple.cpp#L471
    -- This might be simpler as a "table", i.e. @[(String, OS)]@.
    parseOS :: String -> OS
    parseOS =
      let table =
            [ ("ananas"    , Ananas)
            , ("cloudabi"  , CloudABI)
            , ("darwin"    , Darwin)
            , ("dragonfly" , DragonFly)
            , ("freebsd"   , FreeBSD)
            , ("fuchsia"   , Fuchsia)
            , ("ios"       , IOS)
            , ("kfreebsd"  , KFreeBSD)
            , ("linux"     , Linux)
            , ("lv2"       , Lv2)
            , ("macos"     , MacOSX)
            , ("netbsd"    , NetBSD)
            , ("openbsd"   , OpenBSD)
            , ("solaris"   , Solaris)
            , ("win32"     , Win32)
            , ("windows"   , Win32)
            , ("haiku"     , Haiku)
            , ("minix"     , Minix)
            , ("rtems"     , RTEMS)
            , ("nacl"      , NaCl)
            , ("cnk"       , CNK)
            , ("aix"       , AIX)
            , ("cuda"      , CUDA)
            , ("nvcl"      , NVCL)
            , ("amdhsa"    , AMDHSA)
            , ("ps4"       , PS4)
            , ("elfiamcu"  , ELFIAMCU)
            , ("tvos"      , TvOS)
            , ("watchos"   , WatchOS)
            , ("mesa3d"    , Mesa3D)
            , ("contiki"   , Contiki)
            , ("amdpal"    , AMDPAL)
            ]
      in parseFromPrefixTable table UnknownOS

    parseEnvironment :: String -> Environment
    parseEnvironment =
      let table =
            [ ("eabihf"     , EABIHF)
            , ("eabi"       , EABI)
            , ("gnuabin32"  , GNUABIN32)
            , ("gnuabi64"   , GNUABI64)
            , ("gnueabihf"  , GNUEABIHF)
            , ("gnueabi"    , GNUEABI)
            , ("gnux32"     , GNUX32)
            , ("code16"     , CODE16)
            , ("gnu"        , GNU)
            , ("android"    , Android)
            , ("musleabihf" , MuslEABIHF)
            , ("musleabi"   , MuslEABI)
            , ("musl"       , Musl)
            , ("msvc"       , MSVC)
            , ("itanium"    , Itanium)
            , ("cygnus"     , Cygnus)
            , ("amdopencl"  , AMDOpenCL)
            , ("coreclr"    , CoreCLR)
            , ("opencl"     , OpenCL)
            , ("simulator"  , Simulator)
            ]
      in parseFromPrefixTable table UnknownEnvironment

    -- https://github.com/llvm-mirror/llvm/blob/release_60/lib/Support/Triple.cpp#L533
    -- Note: this gets passed the entire environment string.
    parseObjectFormat :: String -> ObjectFormat
    parseObjectFormat =
      let table =
            [ ("coff"  , COFF)
            , ("elf"   , ELF)
            , ("macho" , MachO)
            , ("Wasm"  , Wasm)
            ]
      in parseFromSuffixTable table UnknownObjectFormat
