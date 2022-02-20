-- |
-- Module      : Data.BuffonMachine
-- Description : Buffon machines providing random variates for discrete
--               probability distributions.
-- Copyright   : (c) Maciej Bendkowski, 2022
-- License     : BSD3
-- Maintainer  : maciej.bendkowski@gmail.com
-- Stability   : experimental
--
-- Monad for computations consuming random bits provided by a buffered random
-- bit oracle.
module Data.BuffonMachine (
  BuffonMachine,
  Discrete,
  Oracle (..),
  EvalIO (..),
  eval,
  getBit,
) where

import Control.Monad.Trans.State.Strict (
  State,
  evalState,
  get,
  modify',
  put,
 )

import Data.Bits (Bits (testBit))
import Data.Word (Word32)
import Instances.TH.Lift ()
import System.Random (Random (random), RandomGen, StdGen, getStdGen)
import System.Random.SplitMix (SMGen, initSMGen)

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
  {runBuffonMachine :: State (Oracle g) a}
  deriving (Functor, Applicative, Monad) via State (Oracle g)

class RandomGen g => EvalIO g where
  evalIO :: BuffonMachine g a -> IO a

type Bern g = BuffonMachine g Bool

{-# INLINEABLE getBit #-}
getBit :: RandomGen g => Bern g
getBit = MkBuffonMachine $ do
  modify' regenerate
  oracle <- get
  put $ useBit oracle
  pure $ currentBit oracle

-- |
--  Buffon machine computations resulting in discrete random variables.
type Discrete g = BuffonMachine g Int

-- |
--  Runs the given Buffon machine computation using the given random generator.
{-# INLINEABLE eval #-}
eval :: RandomGen g => BuffonMachine g a -> g -> a
eval m g = evalState (runBuffonMachine m) (fresh g)

instance EvalIO SMGen where
  {-# INLINE evalIO #-}
  evalIO m = eval m <$> initSMGen

instance EvalIO StdGen where
  {-# INLINE evalIO #-}
  evalIO m = eval m <$> getStdGen
