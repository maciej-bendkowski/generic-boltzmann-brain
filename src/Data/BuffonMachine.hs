-- |
-- Module      : Data.BuffonMachine
-- Description : Buffon machines providing random variates for discrete probability distributions.
-- Copyright   : (c) Maciej Bendkowski, 2022
-- License     : BSD3
-- Maintainer  : maciej.bendkowski@gmail.com
-- Stability   : experimental
--
-- Monad for computations consuming random bits provided by a buffered random
-- bit oracle.
--
-- References:
--
-- * Ph. Flajolet, M. Pelletier, M. Soria : "On Buffon Machines and Numbers", SODA'11 (2011).
-- * J. Lumbroso : "Optimal Discrete Uniform Generation from Coin Flips, and Applications".
-- * F. A. Saad, C. E. Freer, M. C. Rinard, V.K. Mansinghka "Optimal Approximate
--     Sampling from Discrete Probability Distributions", POPL'20 (2020).
module Data.BuffonMachine (
  BuffonMachine,
  Discrete,
  Oracle (..),
  run,
  runIO,
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

-- | Buffered random bit oracle.
data Oracle g = Oracle
  { -- | 32-bit buffer of random bits.
    buffer :: !Word32
  , -- | Number of bits consumed from the current buffer.
    usedBits :: !Int
  , -- | Random number generator used to obtain random bits.
    rng :: g
  }

fresh :: RandomGen g => g -> Oracle g
fresh g = case random g of
  (x, g') -> Oracle {buffer = x, usedBits = 0, rng = g'}
{-# INLINE fresh #-}

useBit :: Oracle g -> Oracle g
useBit oracle = oracle {usedBits = succ (usedBits oracle)}
{-# INLINE useBit #-}

currentBit :: Oracle g -> Bool
currentBit oracle = testBit (buffer oracle) (usedBits oracle)
{-# INLINE currentBit #-}

-- |
--  Buffon machines implemented as a `State` monad over `Oracle`.
type BuffonMachine g = Control.Monad.Trans.State.Strict.State (Oracle g)

type Bern g = BuffonMachine g Bool

regenerate :: RandomGen g => Oracle g -> Oracle g
regenerate oracle =
  case usedBits oracle of
    32 -> fresh (rng oracle)
    _ -> oracle
{-# INLINE regenerate #-}

getBit :: RandomGen g => Bern g
getBit = do
  Control.Monad.Trans.State.Strict.modify' regenerate
  oracle <- Control.Monad.Trans.State.Strict.get
  Control.Monad.Trans.State.Strict.put $ useBit oracle
  return $ currentBit oracle

-- |
--  Buffon machine computations resulting in discrete random variables.
type Discrete g = BuffonMachine g Int

-- |
--  Runs the given Buffon machine computation using the given random generator.
run :: RandomGen g => BuffonMachine g a -> g -> a
run m = Control.Monad.Trans.State.Strict.evalState m . fresh

-- |
--  Runs the given Buffon machine computation within the IO monad using StdGen
--  as its random bit generator.
runIO :: BuffonMachine StdGen a -> IO a
runIO m = run m <$> getStdGen
