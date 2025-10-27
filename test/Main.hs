module Main (main) where

import qualified Test.Tasty as Tasty

import qualified Triple
import qualified Output
import qualified DataLayout

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "LLVM tests"
       [
         Output.tests
       , Triple.tests
       , DataLayout.tests
       ]
