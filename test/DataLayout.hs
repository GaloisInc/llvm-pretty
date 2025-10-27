module DataLayout ( tests ) where

import Text.LLVM.AST
import Text.LLVM.PP

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  let rspDL s = Just $ "target datalayout = \"" <> s <> "\""
      ppDL = show . ppLLVM llvmVlatest ppDataLayout
  in testGroup "Test DataLayout"
     [ testCase "datalayout 0" $ ppDL <$> (parseDataLayout "") @?= Just ""
     , testCase "datalayout 1"
       $ let dl = "e" in ppDL <$> (parseDataLayout dl) @?= rspDL dl
     , testCase "datalayout 2"
       $ let dl = "e-m:e" in ppDL <$> (parseDataLayout dl) @?= rspDL dl
     , testCase "datalayout 3"
       $ let dl = "e-m:e-p270:32:32"
         in ppDL <$> (parseDataLayout dl) @?= rspDL dl
     , testCase "datalayout 4"
       $ let dl = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-S128"
         in ppDL <$> (parseDataLayout dl) @?= rspDL dl
     , testCase "datalayout 5"
       $ let dl = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
         in ppDL <$> (parseDataLayout dl) @?= rspDL dl
     , testCase "datalayout 6"
       $ let dl = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-ni:3:4-S128"
         in ppDL <$> (parseDataLayout dl) @?= rspDL dl
     ]
