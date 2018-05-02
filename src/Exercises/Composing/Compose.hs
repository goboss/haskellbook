{-# LANGUAGE InstanceSigs #-}

module Exercises.Composing.Compose where

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

-- Exercise 1
-- Write the Compose Foldable instance.

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap f = foldMap (foldMap f) . getCompose

-- Exercise 2
-- Write the Compose Traversable instance.
instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: Applicative h => (a -> h b) -> Compose f g a -> h (Compose f g b)
  traverse f (Compose fga) = fmap Compose (traverse (traverse f) fga)

-- Exercise 3
-- Write Bifunctor instances for the following types

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b = Deux a b
instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

data Const a b = Const a
instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)

data Drei a b c = Drei a b c
instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c = SuperDrei a b
instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)

data SemiDrei a b c = SemiDrei a
instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d
instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

instance Bifunctor Either where
  bimap f _ (Left a)  = Left (f a)
  bimap _ g (Right b) = Right (g b)
