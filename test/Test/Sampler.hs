module Test.Sampler (tests) where

import Data.Boltzmann (
  BuffonMachine,
  EvalIO (evalIO),
  LowerBound (..),
  UpperBound (..),
  rejectionSampler,
 )

import Test.Samplers.BinTree (BinTree)
import Test.Samplers.Lambda (BinLambda, Lambda, abstractions)
import Test.Samplers.Tree (Tree)

import System.Random.SplitMix (SMGen)

import Control.Monad (replicateM)
import Data.List (genericLength)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (
  testCase,
 )
import Test.Tasty.QuickCheck as QC (testProperty)
import Test.Utils (Size (size), close)

tests :: TestTree
tests =
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
    , testCase "Lambda sampler has the correct output distribution" $ do
        (obsSize, obsAbs) <- runLambdaSampler 1_000
        close obsSize 10_000 0.2 -- just to be sure
        close obsAbs 4_000 0.2 -- just to be sure
    ]

lambdaSampler :: BuffonMachine SMGen Lambda
lambdaSampler = rejectionSampler (MkLowerBound 8_000) (MkUpperBound 12_000)

runLambdaSampler :: Int -> IO (Double, Double)
runLambdaSampler n = evalIO $ do
  sam <- replicateM n lambdaSampler
  pure $ statistics $ (\t -> (size t, abstractions t)) <$> sam

statistics :: [(Int, Int)] -> (Double, Double)
statistics xs = (average $ fst <$> xs, average $ snd <$> xs)

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs
