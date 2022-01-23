module Test.Unit.Sampler (unitTests) where

import Data.Boltzmann.Sampler ()
import Data.Samplers.BinTree
import Data.Samplers.Lambda
import Data.Samplers.Tree
import Test.QuickCheck.Monadic
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import Test.Unit.Utils (size)

unitTests :: TestTree
unitTests =
  testGroup
    "Sampler"
    sampleSizeTests

sampleSizeTests :: [TestTree]
sampleSizeTests =
  [ testProperty "Lambda sampler respects size constraints for sizes around the mean of 1,000" lambdaSamplerSizeProp
  , testProperty "BinTree sampler respects size constraints for sizes around the mean of 1,000" binTreeSamplerSizeProp
  , testProperty "Tree sampler respects size constraints for sizes around the mean of 1,000" treeSamplerSizeProp
  , testProperty "Lambda list sampler respects size constraints for sizes around the mean of 1,000" lambdaListSamplerSizeProp
  ]

lambdaSamplerSizeProp :: Positive Int -> Property
lambdaSamplerSizeProp (Positive x) = monadicIO $ do
  let (lb, ub) = (800 + x, 1200 + x)
  term <- run $ randomLambdaIO lb ub
  let n = size term
  assert $ lb <= n && n <= ub

lambdaListSamplerSizeProp :: Positive Int -> Property
lambdaListSamplerSizeProp (Positive x) = monadicIO $ do
  let (lb, ub) = (800 + x, 1200 + x)
  terms <- run $ randomLambdaListIO lb ub
  let n = size terms
  assert $ lb <= n && n <= ub

binTreeSamplerSizeProp :: Positive Int -> Property
binTreeSamplerSizeProp (Positive x) = monadicIO $ do
  let (lb, ub) = (800 + x, 1200 + x)
  tree <- run $ randomBinTreeIO lb ub
  let n = size tree
  assert $ lb <= n && n <= ub

treeSamplerSizeProp :: Positive Int -> Property
treeSamplerSizeProp (Positive x) = monadicIO $ do
  let (lb, ub) = (800 + x, 1200 + x)
  tree <- run $ randomTreeIO lb ub
  let n = size tree
  assert $ lb <= n && n <= ub
