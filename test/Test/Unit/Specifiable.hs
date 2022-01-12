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
    "Specifiable"
    [typeDefinitionTests, typeNameTests]

typeDefinitionTests :: TestTree
typeDefinitionTests =
  testGroup
    "Type definition tests"
    [ testCase "BinTree is correctly mapped into a type definition" $
        BinTree.expectedTypeDef @=? typedef (undefined :: BinTree),
      testCase "Tree is correctly mapped into a type definition" $
        Tree.expectedTypeDef @=? typedef (undefined :: Tree),
      testCase "DeBruijn is correctly mapped into a type definition" $
        Lambda.expectedDeBruijnTypeDef @=? typedef (undefined :: DeBruijn),
      testCase "Lambda is correctly mapped into a type definition" $
        Lambda.expectedLambdaTypeDef @=? typedef (undefined :: Lambda)
    ]

typeNameTests :: TestTree
typeNameTests =
  testGroup
    "Type name tests"
    [ testCase ("BinTree's type name is '" ++ show ''BinTree ++ "'") $
        show ''BinTree @=? typeName (undefined :: BinTree),
      testCase ("Tree's type name is '" ++ show ''Tree ++ "'") $
        show ''Tree @=? typeName (undefined :: Tree),
      testCase ("Lambda's type name is '" ++ show ''Lambda ++ "'") $
        show ''Lambda @=? typeName (undefined :: Lambda),
      testCase "[DeBruijn]'s type name is [Data.Types.Lambda.DeBruijn]" $
        "[Data.Types.Lambda.DeBruijn]" @=? typeName (undefined :: [DeBruijn]),
      testCase "[[DeBruijn]]'s type name is [[Data.Types.Lambda.DeBruijn]]" $
        "[[Data.Types.Lambda.DeBruijn]]" @=? typeName (undefined :: [[DeBruijn]])
    ]
