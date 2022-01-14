{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Data.Boltzmann.Sampler
-- Description : Boltzmann sampler for specifiable types.
-- Copyright   : (c) Maciej Bendkowski, 2022
-- License     : BSD3
-- Maintainer  : maciej.bendkowski@gmail.com
-- Stability   : experimental
module Data.Boltzmann.Sampler
  ( RejectionSampler,
    DDGSelector,
    WeightSelector,
    BoltzmannSampler (..),
    rejectionSampler,
    rejectionSamplerIO,
  )
where

import Control.Monad (guard)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Boltzmann.Specifiable (Specifiable, listTypeName)
import Data.BuffonMachine (BuffonMachine, DDG, choice, runIO)
import System.Random (RandomGen, StdGen)

-- | Samplers which might "reject" generated objects, i.e. return `Nothing` instead.
type RejectionSampler g a = MaybeT (BuffonMachine g) (a, Int)

-- | Selector functions mapping (fully qualified) constructor names to their `DDG`s.
type DDGSelector = String -> DDG

-- | Selector functions mapping (fully qualified) constructor names to their weights.
type WeightSelector = String -> Int

-- | Multiparametric Boltzmann samplers.
class BoltzmannSampler a where
  sample ::
    RandomGen g =>
    -- | Function mapping constructor names to corresponding `DDG`s.
    DDGSelector ->
    -- | Function mapping constructor names to corresponding weights.
    WeightSelector ->
    -- | Upper bound for the generated objects' weight.
    Int ->
    RejectionSampler g a

instance (Specifiable a, BoltzmannSampler a) => BoltzmannSampler [a] where
  sample ddgs weight ub = do
    guard (ub > 0)
    lift (choice (ddgs $ listTypeName (undefined :: a)))
      >>= ( \case
              0 -> return ([], weight (show '[]))
              _ -> do
                let wcons = weight (show '(:))
                (x, w) <- sample ddgs weight (ub - wcons)
                (xs, ws) <- sample ddgs weight (ub - wcons - w)
                return (x : xs, w + ws + wcons)
          )

-- | Rejection sampler generating objects within a prescribed size window.
rejectionSampler ::
  RandomGen g =>
  -- | Function producing rejection samplers given weight upper bounds.
  (Int -> RejectionSampler g a) ->
  -- | Lower bound for the generated objects' weight.
  Int ->
  -- | Upper bound for the generated objects' weight.
  Int ->
  BuffonMachine g a
rejectionSampler sam lb ub =
  do
    str <- runMaybeT (sam ub)
    case str of
      Nothing -> rejectionSampler sam lb ub
      Just (x, s) ->
        if lb <= s && s <= ub
          then return x
          else rejectionSampler sam lb ub

-- | `IO` variant of `rejectionsampler`.
rejectionSamplerIO ::
  -- | Function producing rejection samplers given weight upper bounds.
  (Int -> RejectionSampler StdGen a) ->
  -- | Lower bound for the generated objects' weight.
  Int ->
  -- | Upper bound for the generated objects' weight.
  Int ->
  IO a
rejectionSamplerIO sam lb ub = runIO (rejectionSampler sam lb ub)
