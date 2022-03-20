{-# LANGUAGE TemplateHaskell #-}

module Data.Boltzmann.Distribution (
  Distribution (..),
  choice,
) where

import Data.BuffonMachine (Discrete, getBit)
import Data.Vector (Vector, null, (!))
import Language.Haskell.TH.Lift (deriveLift)
import System.Random (RandomGen, StdGen)
import System.Random.SplitMix (SMGen)
import Prelude hiding (null)

newtype Distribution = Distribution {unDistribution :: Vector Int}
  deriving stock (Show)

deriveLift ''Distribution

-- |
--  Given a compact discrete distribution generating tree (in vector form)
--  computes a discrete random variable following that distribution.
choice :: RandomGen g => Distribution -> Discrete g
choice enc
  | null (unDistribution enc) = pure 0
  | otherwise = choice' enc 0
{-# SPECIALIZE choice :: Distribution -> Discrete SMGen #-}
{-# SPECIALIZE choice :: Distribution -> Discrete StdGen #-}

choice' :: RandomGen g => Distribution -> Int -> Discrete g
choice' enc c = do
  h <- getBit
  let b = fromEnum h
  let c' = unDistribution enc ! (c + b)
  if unDistribution enc ! c' < 0
    then pure $ -(1 + unDistribution enc ! c')
    else choice' enc c'
