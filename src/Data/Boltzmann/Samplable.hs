{-# LANGUAGE TemplateHaskell #-}

module Data.Boltzmann.Samplable (
  Samplable (..),
  Distribution (..),
  choice,
) where

import Data.BuffonMachine (Discrete, getBit)
import Data.Vector (Vector, null, (!))
import Language.Haskell.TH.Lift (deriveLift)
import System.Random (RandomGen, StdGen)
import System.Random.SplitMix (SMGen)
import Prelude hiding (null)

class Samplable a where
  constrDistribution :: Distribution a

newtype Distribution a = Distribution {unDistribution :: Vector Int}
deriveLift ''Distribution

-- |
--  Given a compact discrete distribution generating tree (in vector form)
--  computes a discrete random variable following that distribution.
choice :: RandomGen g => Distribution a -> Discrete g
choice enc
  | null (unDistribution enc) = return 0
  | otherwise = choice' enc 0
{-# SPECIALIZE choice :: Distribution a -> Discrete SMGen #-}
{-# SPECIALIZE choice :: Distribution a -> Discrete StdGen #-}

choice' :: RandomGen g => Distribution a -> Int -> Discrete g
choice' enc c = do
  h <- getBit
  let b = fromEnum h
  let c' = unDistribution enc ! (c + b)
  if unDistribution enc ! c' < 0
    then return $ -(1 + unDistribution enc ! c')
    else choice' enc c'
