-- |
-- Module      : Data.Boltzmann.Weighed
-- Description :
-- Copyright   : (c) Maciej Bendkowski, 2022
-- License     : BSD3
-- Maintainer  : maciej.bendkowski@gmail.com
-- Stability   : experimental
module Data.Boltzmann.Weighed (
  Weighed (..),
) where

class Weighed a where
  weight :: a -> Int

instance Weighed [a] where
  weight _ = 0
