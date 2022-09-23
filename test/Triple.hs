module Triple (tests) where

import           Control.Monad (forM_)

import qualified Text.LLVM.Triple.AST as AST
import qualified Text.LLVM.Triple.Parse as Parse
import qualified Text.LLVM.Triple.Print as Print

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as TastyH

each :: Bounded a => Enum a => (a -> String) -> (a -> Bool) -> IO ()
each name test =
  forM_ (enumFrom minBound) $ \e ->
    TastyH.assertBool (name e) (test e)

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Triple parse/print"
    [ TastyH.testCase
        "parse . print == id :: Arch -> Arch"
        (roundtrip Parse.parseArch Print.archName)
    , TastyH.testCase
        "parse . print == id :: Vendor -> Vendor"
        (roundtrip Parse.parseVendor Print.vendorName)
    , TastyH.testCase
        "parse . print == id :: OS -> OS"
        (roundtrip Parse.parseOS Print.osName)
    , TastyH.testCase
        "parse . print == id :: Environment -> Environment"
        (roundtrip Parse.parseEnv Print.envName)
    , TastyH.testCase
        "parse . print == id :: ObjectFormat -> ObjectFormat"
        (roundtrip Parse.parseObjFmt Print.objFmtName)
    , triple "aarch64-unknown-linux-gnu" $
        AST.TargetTriple
        { AST.ttArch = AST.AArch64
        , AST.ttSubArch = AST.NoSubArch
        , AST.ttVendor = AST.UnknownVendor
        , AST.ttOS = AST.Linux
        , AST.ttEnv = AST.GNU
        , AST.ttObjFmt = AST.UnknownObjectFormat
        }
    , triple "x86_64-unknown-linux-gnu" $
        AST.TargetTriple
        { AST.ttArch = AST.X86_64
        , AST.ttSubArch = AST.NoSubArch
        , AST.ttVendor = AST.UnknownVendor
        , AST.ttOS = AST.Linux
        , AST.ttEnv = AST.GNU
        , AST.ttObjFmt = AST.UnknownObjectFormat
        }
    ]
  where
    triple s t =
      TastyH.testCase
        ("parse '" ++ s ++ "'")
        (t TastyH.@=? Parse.parseTriple s)
    roundtrip pars prnt =
      each
        (\a -> "parse (print " ++ prnt a ++ ") == " ++ prnt a)
        (\a -> a == pars (prnt a))
