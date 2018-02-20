module Exercises.Foldable.Main where

import Data.Monoid

-- Write Foldable instances for the following datatypes.

-- Exercise 1
data Constant a b = Constant b

instance Foldable (Constant a) where
  foldMap f (Constant x) = f x

-- Exercise 2
data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two _ x) = f x

-- Exercise 3
data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ _ x) = f x

-- Exercise 4
data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ x y) = f x <> f y

-- Exercise 5
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' _ x y z) = f x <> f y <> f z

-- Write a filter function for Foldable types using foldMap.
filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF p = foldMap (\x -> if p x then pure x else mempty)
