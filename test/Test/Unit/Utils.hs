module Test.Unit.Utils (Size (..)) where

class Size a where
  size :: a -> Int
