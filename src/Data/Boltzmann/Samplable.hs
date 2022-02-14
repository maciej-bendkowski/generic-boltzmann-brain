module Data.Boltzmann.Samplable where

import Data.BuffonMachine (Distribution)

class Samplable a where
  constrDistribution :: Distribution a
