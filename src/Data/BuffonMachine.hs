-- |
-- Module      : Data.BuffonMachine
-- Description :
-- Copyright   : (c) Maciej Bendkowski, 2022
-- License     : BSD3
-- Maintainer  : maciej.bendkowski@gmail.com
-- Stability   : experimental
module Data.BuffonMachine
  ( BuffonMachine,
    Discrete,
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

data Oracle g = Oracle
  { buffer :: !Word32,
    usedBits :: !Int,
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

-- | Buffon machines depending on the given random number generator 'g'.
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

-- | Discrete random variables.
type Discrete g = BuffonMachine g Int

-- |
--  Given a compact discrete distribution generating tree (in vector form)
--  computes a discrete random variable following that distribution.
choice :: RandomGen g => Vector Int -> Discrete g
choice enc
  | null enc = return 0 -- note: single-point probability distributions.
  | otherwise = choice' enc 0
{-# SPECIALIZE choice :: Vector Int -> Discrete StdGen #-}

choice' :: RandomGen g => Vector Int -> Int -> Discrete g
choice' enc c = do
  h <- getBit
  let b = fromEnum h
  let c' = enc ! (c + b)
  if enc ! c' < 0 then return $ -(1 + enc ! c') else choice' enc c'

-- |
--  Runs the given Buffon machine computation
--  using the given random generator.
run :: RandomGen g => BuffonMachine g a -> g -> a
run m = evalState m . fresh

-- |
--  Runs the given Buffon machine computation within the IO monad
--  using StdGen as its random bit oracle.
runIO :: BuffonMachine StdGen a -> IO a
runIO m = run m <$> getStdGen
