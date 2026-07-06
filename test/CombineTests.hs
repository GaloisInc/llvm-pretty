module CombineTests
  (
    tests
  )
where

import           Data.Function ( on )
import           Data.String ( fromString )
import           Lens.Micro

import qualified Test.Tasty as Tasty
import           Test.Tasty.HUnit ( assertBool, testCase, (@?=) )

import           Text.LLVM -- ( emptyModule )
import           Text.LLVM.Combine
import           Text.LLVM.Lens


tests :: Tasty.TestTree
tests = Tasty.testGroup "LLVM combine"
  [
    testCase "empty equivalences"
    $ let llvm1 = emptyModule
          llvm2 = emptyModule
          llvm3 = emptyModule
          llvmAll = llvmModuleCombine (llvmModuleCombine llvm1 llvm2) llvm3
      in assertBool "combining empty is empty"
         $ and [ llvm1 == llvm1
               , ((==) `on` (modSourceNameLens .~ Nothing)) llvmAll llvm1
               , ((==) `on` (modSourceNameLens .~ Nothing)) llvmAll llvm2
               , ((==) `on` (modSourceNameLens .~ Nothing)) llvmAll llvm3
               ]

  , testCase "metadata updates"
    $ let llvm1 = emptyModule
                  & modUnnamedMdLens .~ [ UnnamedMd { umIndex = 1
                                                    , umValues = ValMdString "a"
                                                    , umDistinct = False
                                                    }
                                        ]
                  & modNamedMdLens .~ [ NamedMd { nmName = "frog"
                                                , nmValues = [1]
                                                }
                                      ]
          llvm2 = emptyModule
                  & modUnnamedMdLens .~ [ UnnamedMd { umIndex = 1
                                                    , umValues = ValMdString "B"
                                                    , umDistinct = False
                                                    }
                                        , UnnamedMd { umIndex = 2
                                                    , umValues = ValMdRef 1
                                                    , umDistinct = False
                                                    }
                                        ]
                  & modNamedMdLens .~ [ NamedMd { nmName = "pig"
                                                , nmValues = [2, 1]
                                                }
                                      ]
          llvmAll = llvmModuleCombine llvm1 llvm2
    in do llvmAll ^. modUnnamedMdLens @?=
            [ UnnamedMd { umIndex = 1
                        , umValues = ValMdString "a"
                        , umDistinct = False
                        }
            , UnnamedMd { umIndex = 3
                        , umValues = ValMdString "B"
                        , umDistinct = False
                        }
            , UnnamedMd { umIndex = 4
                        , umValues = ValMdRef 3
                        , umDistinct = False
                        }
            ]
          llvmAll ^. modNamedMdLens @?=
            [ NamedMd { nmName = "frog"
                      , nmValues = [1]
                      }
            , NamedMd { nmName = "pig"
                      , nmValues = [4, 3]
                      }
            ]

  , testCase "type name deconflicting"
    $ let llvm1 = emptyModule
            & modTypesLens .~ [ TypeDecl { typeName = fromString "type1"
                                         , typeValue = Opaque }
                              , TypeDecl { typeName = fromString "type1___0"
                                         , typeValue = Alias $ fromString "cow"
                                         }
                              ]
          llvm2 = emptyModule
            & modTypesLens .~ [ TypeDecl { typeName = fromString "type1"
                                         , typeValue = PrimType Void
                                         }
                              , TypeDecl { typeName = fromString "type1___0"
                                         , typeValue = Alias $ fromString "moo"
                                         }
                              , TypeDecl { typeName = fromString "type2"
                                         , typeValue = PtrOpaque
                                         }
                              ]
            & modDefinesLens .~
            [
              Define { defName = fromString "foo"
                     , defLinkage = Nothing
                     , defVisibility = Nothing
                     , defComdat = Nothing
                     , defMetadata = mempty
                     , defGC = Nothing
                     , defSection = Nothing
                     , defVarArgs = False
                     , defArgs =
                         [ Typed { typedType = Alias $ fromString "moo"
                                 , typedValue = fromString "type1___0"
                                 }
                         ]
                     , defRetType = PrimType Void
                     , defAttrs = mempty
                     , defBody = []
                     }
            ]
          llvmAll = llvmModuleCombine llvm1 llvm2
    in do llvmAll ^. modTypesLens @?=
            [ TypeDecl { typeName = fromString "type1"
                       , typeValue = Opaque }
            , TypeDecl { typeName = fromString "type1___0"
                       , typeValue = Alias $ fromString "cow"
                       }
            , TypeDecl { typeName = fromString "type1___1"
                      , typeValue = PrimType Void
                      }
            , TypeDecl { typeName = fromString "type1___0___0"
                       , typeValue = Alias $ fromString "moo"
                       }
            , TypeDecl { typeName = fromString "type2"
                       , typeValue = PtrOpaque
                       }
            ]
          llvmAll ^. modDefinesLens @?=
            [
              Define { defName = fromString "foo"
                     , defLinkage = Nothing
                     , defVisibility = Nothing
                     , defComdat = Nothing
                     , defMetadata = mempty
                     , defGC = Nothing
                     , defSection = Nothing
                     , defVarArgs = False
                     , defArgs =
                         [ Typed { typedType = Alias $ fromString "moo"
                                 , typedValue = fromString "type1___0___0"
                                 }
                         ]
                     , defRetType = PrimType Void
                     , defAttrs = mempty
                     , defBody = []
                     }
            ]
  , testCase "internal define name deconflicting"
    $ let d1 = Define { defName = fromString "foo"
                           , defLinkage = Just Internal
                           , defVisibility = Nothing
                           , defComdat = Nothing
                           , defMetadata = mempty
                           , defGC = Nothing
                           , defSection = Nothing
                           , defVarArgs = False
                           , defArgs =
                             [ Typed { typedType = Alias $ fromString "moo"
                                     , typedValue = fromString "type1"
                                     }
                             ]
                           , defRetType = PrimType $ Integer 8
                           , defAttrs = mempty
                           , defBody = []
                           }
          d2 = Define { defName = fromString "foo"
                      , defLinkage = Just Internal
                      , defVisibility = Nothing
                      , defComdat = Nothing
                      , defMetadata = mempty
                      , defGC = Nothing
                      , defSection = Nothing
                      , defVarArgs = False
                      , defArgs = []
                      , defRetType = PrimType Void
                      , defAttrs = mempty
                      , defBody = []
                           }
          llvm1 = emptyModule & modDefinesLens .~ [ d1 ]
          llvm2 = emptyModule & modDefinesLens .~ [ d2 ]
          llvmAll = llvmModuleCombine llvm1 llvm2
    in do llvmAll ^. modDefinesLens @?=
            [ d1
            , d2 & defNameLens .~ fromString "foo_1"
            ]

  , testCase "declare to define resolution"
    $ let d1 = Declare { decName = fromString "foo"
                       , decLinkage = Nothing
                       , decVisibility = Nothing
                       , decComdat = Nothing
                       , decVarArgs = False
                       , decArgs = [ Alias $ fromString "moo" ]
                       , decRetType = PrimType $ Integer 8
                       , decAttrs = mempty
                       }
          d2 = Define { defName = fromString "foo"
                      , defLinkage = Nothing
                      , defVisibility = Nothing
                      , defComdat = Nothing
                      , defMetadata = mempty
                      , defGC = Nothing
                      , defSection = Nothing
                      , defVarArgs = False
                      , defArgs =
                           [ Typed { typedType = Alias $ fromString "cow"
                                   , typedValue = fromString "type1"
                                   }
                           ]
                      , defRetType = PrimType $ Integer 8
                      , defAttrs = mempty
                      , defBody = []
                           }
          llvm1 = emptyModule & modDeclaresLens .~ [ d1 ]
          llvm2 = emptyModule & modDefinesLens .~ [ d2 ]
          llvmAll = llvmModuleCombine llvm1 llvm2
    in do llvmAll ^. modDefinesLens @?= [ d2 ]
          llvmAll ^. modDeclaresLens @?= []

  ]
