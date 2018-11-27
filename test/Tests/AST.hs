module Tests.AST where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Text.LLVM.AST
import Text.LLVM.AST.Triple

tests :: TestTree
tests = testGroup "AST"
  [ testGroup "Target triples"
      [ let triple = "x86_64-unknown-linux-gnu"
        in testCase ("parse target triple: " ++ triple) $
            parseTargetTriple triple @?=
              TargetTriple
                ( Just X8664
                , Nothing
                , Just (UnknownVendor "unknown")
                , Just Linux
                , Just GNU
                , Just (UnknownObjectFormat "gnu")
                )
      , let triple = "x86_64-apple-macosx10.10.0"
        in testCase ("parse target triple: " ++ triple) $
            parseTargetTriple triple @?=
              TargetTriple
                ( Just X8664
                , Nothing
                , Just Apple
                , Just MacOSX
                , Nothing
                , Nothing
                )
      ]
  ]
