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
module Data.BuffonMachine
  ( BuffonMachine,
    Discrete,
    Oracle (..),
    DDG,
    choice,
    run,
    runIO,
  )
where

import Control.Monad.Trans.State.Strict
  ( State,
    evalState,
    get,
    modify',
    put,
  )
import Data.Bits (Bits (testBit))
import Data.Vector (Vector, null, (!))
import Data.Word (Word32)
import System.Random (Random (random), RandomGen, StdGen, getStdGen)
import Prelude hiding (null)

-- | Buffered random bit oracle.
data Oracle g = Oracle
  { -- | 32-bit buffer of random bits.
    buffer :: !Word32,
    -- | Number of bits consumed from the current buffer.
    usedBits :: !Int,
    -- | Random number generator used to obtain random bits.
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
type BuffonMachine g = State (Oracle g)

type Bern g = BuffonMachine g Bool

regenerate :: RandomGen g => Oracle g -> Oracle g
regenerate oracle =
  case usedBits oracle of
    32 -> fresh (rng oracle)
    _ -> oracle
{-# INLINE regenerate #-}

getBit :: RandomGen g => Bern g
getBit = do
  modify' regenerate
  oracle <- get
  put $ useBit oracle
  return $ currentBit oracle

-- |
--  Buffon machine computations resulting in discrete random variables.
type Discrete g = BuffonMachine g Int

-- |
--   Discrete distribution generating trees in compact vector form.
--   Trivial, single-point distributions are encoded as empty vectors.
type DDG = Vector Int

-- |
--  Given a compact discrete distribution generating tree (in vector form)
--  computes a discrete random variable following that distribution.
choice :: RandomGen g => DDG -> Discrete g
choice enc
  | null enc = return 0
  | otherwise = choice' enc 0
{-# SPECIALIZE choice :: DDG -> Discrete StdGen #-}

choice' :: RandomGen g => DDG -> Int -> Discrete g
choice' enc c = do
  h <- getBit
  let b = fromEnum h
  let c' = enc ! (c + b)
  if enc ! c' < 0 then return $ -(1 + enc ! c') else choice' enc c'

-- |
--  Runs the given Buffon machine computation using the given random generator.
run :: RandomGen g => BuffonMachine g a -> g -> a
run m = evalState m . fresh

-- |
--  Runs the given Buffon machine computation within the IO monad using StdGen
--  as its random bit generator.
runIO :: BuffonMachine StdGen a -> IO a
runIO m = run m <$> getStdGen
