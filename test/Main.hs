module Main (main) where

import qualified Test.Tasty as Tasty

import qualified Triple

main :: IO ()
main = Tasty.defaultMain Triple.tests
