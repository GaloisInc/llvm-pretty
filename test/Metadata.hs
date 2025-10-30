{-# LANGUAGE TypeApplications #-}

-- | This module provides some testing of the metadata that can be attached to
-- Statements.

module Metadata ( tests ) where

import qualified Test.Tasty as Tasty
import           Test.Tasty.HUnit

import           Text.LLVM.AST


tests :: Tasty.TestTree
tests = Tasty.testGroup "LLVM metadata tests" $
  let s1 = Effect (Load PtrOpaque (Typed Opaque ValNull) Nothing Nothing)
           mempty md1
      s2 = Result (Ident "r1")
           (Load PtrOpaque (Typed Opaque ValNull) Nothing Nothing)
           mempty md1
      md1 = [ ("foo", ValMdLoc $ DebugLoc { dlLine = 12
                                           , dlCol = 34
                                           , dlScope = ValMdRef 5
                                           , dlIA = Nothing
                                           , dlImplicit = True })
             , ("moo", ValMdString @Value "cow")
             ]
  in
  [
    testCase "stmtMetadata Effect" $ stmtMetadata s1 @?= md1
  , testCase "stmtMetadata Result" $ stmtMetadata s2 @?= md1
  ]
