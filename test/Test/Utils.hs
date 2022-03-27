module Test.Utils (Size(..)) where

-- | Objects with size.
class Size a where
  size :: a -> Int
