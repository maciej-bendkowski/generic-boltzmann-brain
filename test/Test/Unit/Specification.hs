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
    "Specification unit tests"
    [collectTypesTests, getWeightTests, getFrequencyTests]

collectTypesTests :: TestTree
collectTypesTests =
  testGroup
    "Type collection unit tests"
    [ testCase "BinTree's types are collected correctly" $
        Set.singleton BinTree.binTree
          @=? Specification.collectTypes BinTree.binTreeSysSpec,
      testCase "Lambda's types are collected correctly" $
        Set.fromList [Lambda.lambda, Lambda.deBruijn]
          @=? Specification.collectTypes Lambda.lambdaSysSpec,
      testCase "Trees's types are collected correctly" $
        Set.fromList [Tree.tree, Tree.treeList]
          @=? Specification.collectTypes Tree.treeSysSpec,
      testCase "Custom's types are collected correctly" $
        Set.fromList [Custom.custom, Custom.custom', Custom.customList']
          @=? Specification.collectTypes Custom.customSysSpec
    ]

getWeightTests :: TestTree
getWeightTests =
  testGroup
    "Constructor weight unit tests"
    [ testCase "BinTree's constructor weights are computed correctly" $ do
        0 @=? BinTree.binTreeSysSpec `Specification.getWeight` show 'BinTree.Leaf
        1 @=? BinTree.binTreeSysSpec `Specification.getWeight` show 'BinTree.Node,
      testCase "Lambda's constructor weights are computed correctly" $ do
        0 @=? Lambda.lambdaSysSpec `Specification.getWeight` show 'Lambda.Index
        1 @=? Lambda.lambdaSysSpec `Specification.getWeight` show 'Lambda.Abs
        1 @=? Lambda.lambdaSysSpec `Specification.getWeight` show 'Lambda.App
        1 @=? Lambda.lambdaSysSpec `Specification.getWeight` show 'Lambda.S
        1 @=? Lambda.lambdaSysSpec `Specification.getWeight` show 'Lambda.Z,
      testCase "Tree's constructor weights are computed correctly" $ do
        1 @=? Tree.treeSysSpec `Specification.getWeight` show 'Tree.Node
        0 @=? Tree.treeSysSpec `Specification.getWeight` show '[]
        0 @=? Tree.treeSysSpec `Specification.getWeight` show '(:),
      testCase "Custom's constructor weights are computed correctly" $ do
        2 @=? Custom.customSysSpec `Specification.getWeight` show 'Custom.ConsA
        3 @=? Custom.customSysSpec `Specification.getWeight` show 'Custom.ConsB
        4 @=? Custom.customSysSpec `Specification.getWeight` show 'Custom.ConsC
    ]

getFrequencyTests :: TestTree
getFrequencyTests =
  testGroup
    "Constructor frequencies unit tests"
    [ testCase "BinTree's constructor frequencies are computed correctly" $ do
        Nothing @=? BinTree.binTreeSysSpec `Specification.getFrequency` show 'BinTree.Leaf
        Nothing @=? BinTree.binTreeSysSpec `Specification.getFrequency` show 'BinTree.Node,
      testCase "Lambda's constructor frequencies are computed correctly" $ do
        Nothing @=? Lambda.lambdaSysSpec `Specification.getFrequency` show 'Lambda.Index
        Nothing @=? Lambda.lambdaSysSpec `Specification.getFrequency` show 'Lambda.Abs
        Nothing @=? Lambda.lambdaSysSpec `Specification.getFrequency` show 'Lambda.App
        Nothing @=? Lambda.lambdaSysSpec `Specification.getFrequency` show 'Lambda.S
        Nothing @=? Lambda.lambdaSysSpec `Specification.getFrequency` show 'Lambda.Z,
      testCase "Tree's constructor frequencies are computed correctly" $ do
        Nothing @=? Tree.treeSysSpec `Specification.getFrequency` show 'Tree.Node
        Nothing @=? Tree.treeSysSpec `Specification.getFrequency` show '[]
        Nothing @=? Tree.treeSysSpec `Specification.getFrequency` show '(:),
      testCase "Custom's constructor frequencies are computed correctly" $ do
        Just 800 @=? Custom.customSysSpec `Specification.getFrequency` show 'Custom.ConsA
        Just 900 @=? Custom.customSysSpec `Specification.getFrequency` show 'Custom.ConsB
        Nothing @=? Custom.customSysSpec `Specification.getFrequency` show 'Custom.ConsC
    ]
