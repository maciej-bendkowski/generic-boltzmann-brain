{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Data.Boltzmann.Sampler
-- Description :
-- Copyright   : (c) Maciej Bendkowski, 2022
-- License     : BSD3
-- Maintainer  : maciej.bendkowski@gmail.com
-- Stability   : experimental
module Data.Boltzmann.Sampler
  ( RejectionSampler,
    BoltzmannSampler (..),
    rejectionSampler,
    rejectionSamplerIO,
  )
where

import Control.Monad (guard)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Boltzmann.Specifiable (Specifiable, listTypeName)
import Data.BuffonMachine (BuffonMachine, choice, runIO)
import Data.Vector (Vector)
import System.Random (RandomGen, StdGen)

type RejectionSampler g a = MaybeT (BuffonMachine g) (a, Int)

class BoltzmannSampler a where
  sample :: RandomGen g => (String -> Vector Int) -> (String -> Int) -> Int -> RejectionSampler g a

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

rejectionSampler :: RandomGen g => (Int -> RejectionSampler g a) -> Int -> Int -> BuffonMachine g a
rejectionSampler sam lb ub =
  do
    str <- runMaybeT (sam ub)
    case str of
      Nothing -> rejectionSampler sam lb ub
      Just (x, s) ->
        if lb <= s && s <= ub
          then return x
          else rejectionSampler sam lb ub

rejectionSamplerIO :: (Int -> RejectionSampler StdGen a) -> Int -> Int -> IO a
rejectionSamplerIO sam lb ub = runIO (rejectionSampler sam lb ub)
