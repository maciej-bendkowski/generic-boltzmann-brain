-- |
-- Module      : Data.Boltzmann.Sampler
-- Description : Boltzmann sampler for specifiable types.
-- Copyright   : (c) Maciej Bendkowski, 2022
-- License     : BSD3
-- Maintainer  : maciej.bendkowski@gmail.com
-- Stability   : experimental
module Data.Boltzmann.Sampler (
  BoltzmannSampler (..),
  rejectionSampler,
  hoistBoltzmannSampler,
) where

import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.BuffonMachine (BuffonMachine, eval)
import System.Random (RandomGen)
import Test.QuickCheck (Gen)
import Test.QuickCheck.Gen (Gen (MkGen))
import Test.QuickCheck.Random (QCGen (QCGen))

-- | Multiparametric Boltzmann samplers.
class BoltzmannSampler a where
  sample :: RandomGen g => Int -> MaybeT (BuffonMachine g) (a, Int)

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

hoistBoltzmannSampler :: BoltzmannSampler a => Gen a
hoistBoltzmannSampler = MkGen $ \(QCGen g) n ->
  let machine = rejectionSampler 0 n
   in eval machine g
