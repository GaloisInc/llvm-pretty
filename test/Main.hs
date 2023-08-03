module Main (main) where

import qualified Test.Tasty as Tasty

import qualified Triple
import qualified Output

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "LLVM tests"
       [
         Output.tests
       , Triple.tests
       ]
