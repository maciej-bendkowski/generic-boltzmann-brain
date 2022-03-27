module Test.Unit.Sampler (unitTests) where

import Test.Samplers.BinTree (BinTree)
import Test.Samplers.Lambda (BinLambda, Lambda)
import Test.Samplers.Tree (Tree)
import Test.Utils (Size (size))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck as QC (testProperty)

unitTests :: TestTree
unitTests =
  testGroup
    "Sampler tests"
    [ QC.testProperty "BinTree sampler respects size constraints" $
        \binTree ->
          let s = size @BinTree binTree
           in 800 <= s && s <= 1200
    , QC.testProperty "Tree sampler respects size constraints" $
        \tree ->
          let s = size @Tree tree
           in 1600 <= s && s <= 2400
    , QC.testProperty "Lambda sampler respects size constraints" $
        \term ->
          let s = size @Lambda term
           in 8_000 <= s && s <= 12_000
    , QC.testProperty "BinLambda sampler respects size constraints" $
        \term ->
          let s = size @BinLambda term
           in 5_000 <= s && s <= 6_400
    ]
