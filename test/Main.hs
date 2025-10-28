module Main (main) where

import qualified Test.Tasty as Tasty

import qualified DataLayout
import qualified Metadata
import qualified Output
import qualified Triple

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "LLVM tests"
       [
         DataLayout.tests
       , Metadata.tests
       , Output.tests
       , Triple.tests
       ]
