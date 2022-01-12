{-# LANGUAGE TemplateHaskellQuotes #-}

module Test.Unit.Specification
  ( unitTests,
  )
where

import qualified Data.Boltzmann.Specification as Specification
import qualified Data.Set as Set
import qualified Data.Types.BinTree as BinTree
import qualified Data.Types.Custom as Custom
import qualified Data.Types.Lambda as Lambda
import qualified Data.Types.Tree as Tree
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( testCase,
    (@=?),
  )

unitTests :: TestTree
unitTests =
  testGroup
    "Specification"
    (collectTypesTests ++ getWeightTests)

collectTypesTests :: [TestTree]
collectTypesTests =
  [ testCase "Types are correctly collected from BinTree's system specification" $
      Set.singleton BinTree.binTree
        @=? Specification.collectTypes BinTree.binTreeSysSpec,
    testCase "Types are correctly collected from Lambda's system specification" $
      Set.fromList [Lambda.lambda, Lambda.deBruijn]
        @=? Specification.collectTypes Lambda.lambdaSysSpec,
    testCase "Types are correctly collected from Trees's system specification" $
      Set.fromList [Tree.tree, Tree.treeList]
        @=? Specification.collectTypes Tree.treeSysSpec,
    testCase "Types are correctly collected from Custom's system specification" $
      Set.fromList [Custom.custom, Custom.custom', Custom.customList']
        @=? Specification.collectTypes Custom.customSysSpec
  ]

getWeightTests :: [TestTree]
getWeightTests =
  [ testCase "Constructor weights are correctly computed from BinTree's system specification" $ do
      0 @=? BinTree.binTreeSysSpec `Specification.getWeight` show 'BinTree.Leaf
      1 @=? BinTree.binTreeSysSpec `Specification.getWeight` show 'BinTree.Node,
    testCase "Constructor weights are correctly computed from Lambda's system specification" $ do
      0 @=? Lambda.lambdaSysSpec `Specification.getWeight` show 'Lambda.Index
      1 @=? Lambda.lambdaSysSpec `Specification.getWeight` show 'Lambda.Abs
      1 @=? Lambda.lambdaSysSpec `Specification.getWeight` show 'Lambda.App
      1 @=? Lambda.lambdaSysSpec `Specification.getWeight` show 'Lambda.S
      1 @=? Lambda.lambdaSysSpec `Specification.getWeight` show 'Lambda.Z,
    testCase "Constructor weights are correctly computed from Tree's system specification" $ do
      1 @=? Tree.treeSysSpec `Specification.getWeight` show 'Tree.Node
      0 @=? Tree.treeSysSpec `Specification.getWeight` show '[]
      0 @=? Tree.treeSysSpec `Specification.getWeight` show '(:),
    testCase "Constructor weights are correctly computed from Custom's system specification" $ do
      2 @=? Custom.customSysSpec `Specification.getWeight` show 'Custom.ConsA
      3 @=? Custom.customSysSpec `Specification.getWeight` show 'Custom.ConsB
      4 @=? Custom.customSysSpec `Specification.getWeight` show 'Custom.ConsC
  ]
