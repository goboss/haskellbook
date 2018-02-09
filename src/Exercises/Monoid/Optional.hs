module Exercises.Monoid.Optional where

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Only (mempty)

  mappend x Nada  = x
  mappend Nada x = x
  mappend (Only x) (Only y) = Only (mappend x y)

