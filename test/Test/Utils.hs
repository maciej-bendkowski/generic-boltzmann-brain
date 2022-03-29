module Test.Utils (Size (..), almostEqual, close) where

import Test.Tasty.HUnit (Assertion, assertBool)

-- | Objects with size.
class Size a where
  size :: a -> Int

almostEqual :: (Show a, Ord a, Fractional a) => a -> a -> Assertion
almostEqual a b =
  assertBool ("Was " ++ show a ++ " expected almost " ++ show b) $
    abs (a - b) < 0.01

close :: (Show a, Ord a, Num a) => a -> a -> a -> Assertion
close a b eps =
  assertBool ("Was " ++ show a ++ " expected close to " ++ show b) $
    (1 - eps) * b <= a && a <= (1 + eps) * b
