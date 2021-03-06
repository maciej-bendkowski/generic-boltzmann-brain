{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Data.Boltzmann.BuffonMachine
-- Description :
-- Copyright   : (c) Maciej Bendkowski, 2022
-- License     : BSD3
-- Maintainer  : maciej.bendkowski@gmail.com
-- Stability   : experimental
module Data.Boltzmann.BuffonMachine (
  -- * Buffon machines
  BuffonMachine,
  Oracle,
  EvalIO (..),
  eval,

  -- * Random variables
  Discrete,
  Bern,

  -- * Discrete distribution generating trees
  Distribution (..),
  choice,
) where

import Control.Monad.Trans.State.Strict (
  State,
  evalState,
  get,
  modify',
  put,
 )

import Data.Bits (Bits (testBit))
import Data.Vector (Vector, null, (!))
import Data.Word (Word32)
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (deriveLift)
import System.Random (Random (random), RandomGen, StdGen, getStdGen)
import System.Random.SplitMix (SMGen, initSMGen)
import Prelude hiding (null)

-- | Buffered random bit oracle.
data Oracle g = Oracle
  { -- | 32-bit buffer of random bits.
    buffer :: !Word32
  , -- | Number of bits consumed from the current buffer.
    usedBits :: !Int
  , -- | Random number generator used to obtain random bits.
    rng :: g
  }

{-# INLINE fresh #-}
fresh :: RandomGen g => g -> Oracle g
fresh g = case random g of
  (x, g') -> Oracle {buffer = x, usedBits = 0, rng = g'}

{-# INLINE useBit #-}
useBit :: Oracle g -> Oracle g
useBit oracle = oracle {usedBits = succ (usedBits oracle)}

{-# INLINE currentBit #-}
currentBit :: Oracle g -> Bool
currentBit oracle = testBit (buffer oracle) (usedBits oracle)

{-# INLINE regenerate #-}
regenerate :: RandomGen g => Oracle g -> Oracle g
regenerate oracle =
  case usedBits oracle of
    32 -> fresh (rng oracle)
    _ -> oracle

-- |
--  Buffon machines implemented as a `State` monad over `Oracle`.
newtype BuffonMachine g a = MkBuffonMachine
  {runBitOracle :: State (Oracle g) a}
  deriving (Functor, Applicative, Monad) via State (Oracle g)

-- |
--  Buffon machines computing Bernoulli random variables.
type Bern g = BuffonMachine g Bool

{-# INLINEABLE getBit #-}
getBit :: RandomGen g => Bern g
getBit = MkBuffonMachine $ do
  modify' regenerate
  oracle <- get
  put $ useBit oracle
  pure $ currentBit oracle

-- |
--  Buffon machines computing discrete random variables.
type Discrete g = BuffonMachine g Int

-- |
--  Runs the Buffon machine using the given random generator.
{-# INLINEABLE eval #-}
eval :: RandomGen g => BuffonMachine g a -> g -> a
eval m g = evalState (runBitOracle m) (fresh g)

-- |
--  Random generators which can be lifted to IO
--  and used to run Buffon machine computations.
class RandomGen g => EvalIO g where
  evalIO :: BuffonMachine g a -> IO a

instance EvalIO SMGen where
  {-# INLINE evalIO #-}
  evalIO m = eval m <$> initSMGen

instance EvalIO StdGen where
  {-# INLINE evalIO #-}
  evalIO m = eval m <$> getStdGen

-- | Discrete distribution generating trees in vector form.
newtype Distribution = MkDistribution {unDistribution :: Vector Int}
  deriving stock (Show)

deriveLift ''Distribution

-- |
--  Given a compact discrete distribution generating tree in vector form,
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
