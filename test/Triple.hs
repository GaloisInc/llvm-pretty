module Triple (tests) where

import qualified Text.LLVM.Triple.AST ()
import qualified Text.LLVM.Triple.Parse as Parse
import qualified Text.LLVM.Triple.Print as Print

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as TastyH

each :: Bounded a => Enum a => (a -> Bool) -> Bool
each f = all f (enumFrom minBound)

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Triple parse/print"
    [ assert
        "parse . print == id :: Vendor -> Vendor"
        (roundtrip Parse.parseVendor Print.vendorName)
    , assert
        "parse . print == id :: OS -> OS"
        (roundtrip Parse.parseOS Print.osName)
    , assert
        "parse . print == id :: Environment -> Environment"
        (roundtrip Parse.parseEnv Print.envName)
    ]
  where
    assert s b = TastyH.testCase s (TastyH.assertBool s b)
    roundtrip pars prnt = each (\a -> a == pars (prnt a))
