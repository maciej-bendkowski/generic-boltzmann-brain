module Test.Unit.Utils (Size (..)) where

class Size a where
  size :: a -> Int

instance Size a => Size [a] where
  size = sum . map size
