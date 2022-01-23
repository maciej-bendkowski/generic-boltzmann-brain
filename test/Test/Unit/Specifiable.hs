{-# LANGUAGE TemplateHaskellQuotes #-}

module Test.Unit.Specifiable (unitTests) where

import Data.Boltzmann.Specifiable (typeName, typedef)
import Data.Types.BinTree (BinTree)
import qualified Data.Types.BinTree as BinTree
import Data.Types.Lambda (DeBruijn, Lambda)
import qualified Data.Types.Lambda as Lambda
import Data.Types.Tree (Tree)
import qualified Data.Types.Tree as Tree
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

unitTests :: TestTree
unitTests =
  testGroup
    "Specifiable unit tests"
    [typeDefinitionTests, typeNameTests]

typeDefinitionTests :: TestTree
typeDefinitionTests =
  testGroup
    "Type definition unit tests"
    [ testCase "BinTree has a correct type definition" $
        BinTree.expectedTypeDef @=? typedef (undefined :: BinTree)
    , testCase "Tree has a correct type definition" $
        Tree.expectedTypeDef @=? typedef (undefined :: Tree)
    , testCase "DeBruijn has a correct type definition" $
        Lambda.expectedDeBruijnTypeDef @=? typedef (undefined :: DeBruijn)
    , testCase "Lambda has a correct type definition" $
        Lambda.expectedLambdaTypeDef @=? typedef (undefined :: Lambda)
    ]

typeNameTests :: TestTree
typeNameTests =
  testGroup
    "Type name unit tests"
    [ testCase "BinTree's type name is correct" $
        show ''BinTree @=? typeName (undefined :: BinTree)
    , testCase "Tree's type name is correct" $
        show ''Tree @=? typeName (undefined :: Tree)
    , testCase "Lambda's type name is correct" $
        show ''Lambda @=? typeName (undefined :: Lambda)
    , testCase "[DeBruijn]'s type name is correct" $
        "[Data.Types.Lambda.DeBruijn]" @=? typeName (undefined :: [DeBruijn])
    , testCase "[[DeBruijn]]'s type name is correct" $
        "[[Data.Types.Lambda.DeBruijn]]" @=? typeName (undefined :: [[DeBruijn]])
    ]
