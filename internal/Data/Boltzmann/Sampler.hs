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
  rejectionSampler,
  toleranceRejectionSampler,

  -- * Other utilities
  hoistBoltzmannSampler,
) where

import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Boltzmann.BuffonMachine (BuffonMachine, eval)
import System.Random (RandomGen)
import Test.QuickCheck (Gen)
import Test.QuickCheck.Gen (Gen (MkGen))
import Test.QuickCheck.Random (QCGen (QCGen))

-- | Multiparametric Boltzmann samplers.
class BoltzmannSampler a where
  -- |
  --  Samples a random object of type @a@. If the object size is larger than
  --  the given upper bound parameter, @Nothing@ is returned instead.
  sample :: RandomGen g => Int -> MaybeT (BuffonMachine g) (a, Int)

-- |
--  Rejection sampler for type @a@. Given lower and upper bound @lb@ and @ub@
--  generates a random objects of size on between @lb@ and @ub@.
rejectionSampler ::
  (RandomGen g, BoltzmannSampler a) => Int -> Int -> BuffonMachine g a
rejectionSampler lb ub = do
  runMaybeT (sample ub)
    >>= ( \case
            Just (obj, s) ->
              if lb <= s && s <= ub
                then pure obj
                else rejectionSampler lb ub
            Nothing -> rejectionSampler lb ub
        )

-- |
--  Rejection sampler for type @a@ which uses a given @eps@ parameter to
--  determine the admissible size window @[(1-eps) N, (1+eps) N]@ for a
--  sampler centered around the target mean size @N@.
toleranceRejectionSampler ::
  (RandomGen g, BoltzmannSampler a) => Int -> Double -> BuffonMachine g a
toleranceRejectionSampler n eps = rejectionSampler lb ub
  where
    lb = floor $ (1 - eps) * fromIntegral n
    ub = ceiling $ (1 + eps) * fromIntegral n

-- |
--  Hoists a given Boltzmann sampler for @a@ into
--  a Quickcheck generator @Gen a@.
hoistBoltzmannSampler :: BoltzmannSampler a => Gen a
hoistBoltzmannSampler = MkGen $ \(QCGen g) n ->
  let machine = rejectionSampler 0 n
   in eval machine g
