-- |
-- Module      : Data.Boltzmann.Sampler
-- Description :
-- Copyright   : (c) Maciej Bendkowski, 2022
-- License     : BSD3
-- Maintainer  : maciej.bendkowski@gmail.com
-- Stability   : experimental
module Data.Boltzmann.Sampler (
  -- * Boltzmann samplers
  BoltzmannSampler (..),

  -- * Rejection samplers
  LowerBound (..),
  UpperBound (..),
  rejectionSampler,
  toleranceRejectionSampler,

  -- * Other utilities
  quickCheckRejectionSampler,
  quickCheckToleranceRejectionSampler,
) where

import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Boltzmann.BuffonMachine (BuffonMachine, eval)
import Data.Coerce (coerce)
import System.Random (RandomGen)

import Data.Boltzmann.System (MeanSize)
import Data.Boltzmann.System.TH (LowerBound (..), UpperBound (..))
import qualified Test.QuickCheck as QuickCheck (Gen)
import qualified Test.QuickCheck.Gen as QuickCheck (Gen (MkGen))
import qualified Test.QuickCheck.Random as QuickCheck (QCGen (QCGen))

-- | Multiparametric Boltzmann samplers.
class BoltzmannSampler a where
  -- |
  --  Samples a random object of type @a@. If the object size is larger than
  --  the given upper bound parameter, @Nothing@ is returned instead.
  sample :: RandomGen g => UpperBound -> MaybeT (BuffonMachine g) (a, Int)

-- |
--  Rejection sampler for type @a@. Given lower and upper bound @lb@ and @ub@
--  generates a random objects of size on between @lb@ and @ub@.
rejectionSampler ::
  (RandomGen g, BoltzmannSampler a) =>
  LowerBound ->
  UpperBound ->
  BuffonMachine g a
rejectionSampler lb ub = do
  runMaybeT (sample $ coerce ub)
    >>= ( \case
            Just (obj, s) ->
              if coerce lb <= s && s <= coerce ub
                then pure obj
                else rejectionSampler lb ub
            Nothing -> rejectionSampler lb ub
        )

-- |
--  Rejection sampler for type @a@ which uses a given @eps@ parameter to
--  determine the admissible size window @[(1-eps) n, (1+eps) n]@ centered
--  around the given size @n@.
toleranceRejectionSampler ::
  (RandomGen g, BoltzmannSampler a) => MeanSize -> Double -> BuffonMachine g a
toleranceRejectionSampler n eps = rejectionSampler lb ub
  where
    lb = MkLowerBound $ floor $ (1 - eps) * fromIntegral n
    ub = MkUpperBound $ ceiling $ (1 + eps) * fromIntegral n

-- |
--  Using the given tolerance, maps a tolerance rejection sampler
--  for @a@ to a Quickcheck generator @Gen a@.
quickCheckRejectionSampler ::
  BoltzmannSampler a =>
  (Int -> (LowerBound, UpperBound)) ->
  QuickCheck.Gen a
quickCheckRejectionSampler genBounds = QuickCheck.MkGen $
  \(QuickCheck.QCGen g) n ->
    let (lb, ub) = genBounds n
        machine = rejectionSampler lb ub
     in eval machine g

-- |
--  Using the given tolerance, maps a tolerance rejection sampler
--  for @a@ to a Quickcheck generator @Gen a@.
quickCheckToleranceRejectionSampler ::
  BoltzmannSampler a =>
  Double ->
  QuickCheck.Gen a
quickCheckToleranceRejectionSampler eps = QuickCheck.MkGen $
  \(QuickCheck.QCGen g) n ->
    let machine = toleranceRejectionSampler n eps
     in eval machine g
