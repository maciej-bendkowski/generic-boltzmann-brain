module Test.BuffonMachine (tests) where

import Control.Monad (replicateM)
import Data.Boltzmann.BuffonMachine (Distribution (..), choice, evalIO)
import qualified Data.Map as Map
import Data.Vector (fromList)
import System.Random.SplitMix (SMGen)
import Test.Tasty (
  TestTree,
  testGroup,
 )
import Test.Tasty.HUnit (
  testCase,
 )
import Test.Utils (almostEqual)

tests :: TestTree
tests =
  testGroup
    "BuffonMachine tests"
    [choiceTests]

choiceTests :: TestTree
choiceTests =
  testGroup
    "Choice tests"
    [ testCase "[1/2, 1/2] is correctly sampled from" $ do
        [(0, a), (1, b)] <- choiceTest distributionA 1_000_000
        a `almostEqual` 0.5
        b `almostEqual` 0.5
    , testCase "[1/3, 1/3, 1/3] is correctly sampled from" $ do
        [(0, a), (1, b), (2, c)] <- choiceTest distributionB 1_000_000
        a `almostEqual` 0.33
        b `almostEqual` 0.33
        c `almostEqual` 0.33
    , testCase "[1/7, 4/7, 2/7] is correctly sampled from" $ do
        [(0, a), (1, b), (2, c)] <- choiceTest distributionC 1_000_000
        a `almostEqual` 0.14
        b `almostEqual` 0.57
        c `almostEqual` 0.28
    , testCase "[1/1] is correctly sampled from" $ do
        [(0, a)] <- choiceTest distributionD 1_000_000
        a `almostEqual` 1.0
    ]

-- [1/2, 1/2]
distributionA :: Distribution
distributionA = MkDistribution $ fromList [2, 3, -2, -1]

-- [1/3, 1/3, 1/3]
distributionB :: Distribution
distributionB = MkDistribution $ fromList [2, 138, 4, 137, 6, 133, 8, 132, 10, 128, 12, 127, 14, 123, 16, 122, 18, 118, 20, 117, 22, 113, 24, 112, 26, 108, 28, 107, 30, 103, 32, 102, 34, 98, 36, 97, 38, 93, 40, 92, 42, 88, 44, 87, 46, 83, 48, 82, 50, 78, 52, 77, 54, 73, 56, 72, 58, 68, 60, 67, 62, 66, 64, 65, -2, -1, -3, -3, 70, 71, -2, -1, -3, 75, 76, -2, -1, -3, 80, 81, -2, -1, -3, 85, 86, -2, -1, -3, 90, 91, -2, -1, -3, 95, 96, -2, -1, -3, 100, 101, -2, -1, -3, 105, 106, -2, -1, -3, 110, 111, -2, -1, -3, 115, 116, -2, -1, -3, 120, 121, -2, -1, -3, 125, 126, -2, -1, -3, 130, 131, -2, -1, -3, 135, 136, -2, -1, -3, 140, 141, -2, -1]

-- [1/7, 4/7, 2/7]
distributionC :: Distribution
distributionC = MkDistribution $ fromList [2, 96, 4, 95, 6, 94, 8, 93, 10, 92, 12, 91, 14, 90, 16, 89, 18, 88, 20, 87, 22, 86, 24, 85, 26, 84, 28, 83, 30, 82, 32, 81, 34, 80, 36, 79, 38, 78, 40, 77, 42, 76, 44, 75, 46, 74, 48, 73, 50, 72, 52, 71, 54, 70, 56, 69, 58, 68, 60, 67, 62, 66, 64, 65, -3, -1, -2, -1, -3, -2, -1, -3, -2, -1, -3, -2, -1, -3, -2, -1, -3, -2, -1, -3, -2, -1, -3, -2, -1, -3, -2, -1, -3, -2, -1, -3, -2]

distributionD :: Distribution
distributionD = MkDistribution $ fromList []

choiceTest :: Distribution -> Int -> IO [(Int, Double)]
choiceTest dist n = evalIO $ do
  sam <- replicateM n (choice @SMGen dist)
  let groups = frequency sam
  pure $ map (\(k, s) -> (k, fromIntegral s / fromIntegral n)) groups

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = Map.toList (Map.fromListWith (+) [(x, 1) | x <- xs])
