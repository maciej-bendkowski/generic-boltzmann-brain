module Test.Unit.Sampler (unitTests) where

import qualified Test.Samplers.BinTree as BinTree
import qualified Test.Samplers.Lambda as Lambda
import qualified Test.Samplers.Tree as Tree

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck as QC (testProperty)

unitTests :: TestTree
unitTests =
  testGroup
    "Sampler tests"
    [ QC.testProperty "BinTree sampler respects size constraints" $
        \binTree ->
          let s = BinTree.size binTree
           in 800 <= s && s <= 1200
    , QC.testProperty "Tree sampler respects size constraints" $
        \tree ->
          let s = Tree.size tree
           in 1600 <= s && s <= 2400
    , QC.testProperty "Lambda sampler respects size constraints" $
        \term ->
          let s = Lambda.size term
           in 8_000 <= s && s <= 12_000
    , QC.testProperty "BinLambda sampler respects size constraints" $
        \term ->
          let s = Lambda.sizeBin term
           in 5_000 <= s && s <= 6_400
    ]