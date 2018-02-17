module Exercises.Monad.Either where

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f s = pure f <*> s

instance Applicative (Sum a) where
  pure = Second
  (First a) <*> _          = First a
  (Second _) <*> (First a) = First a
  (Second f) <*> (Second x) = Second (f x)

instance Monad (Sum a) where
  return = pure
  (First a)  >>= _ = First a
  (Second b) >>= f = f b
