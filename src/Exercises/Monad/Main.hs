module Exercises.Monad.Main where

import Prelude hiding (Left, Right)

import Control.Monad (join)

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Write Monad instances for the following types.
-- Use the QuickCheck properties we showed you to validate your instances.

-- Exercise 1
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _    = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return    = pure
  (>>=) _ _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

-- Exercise 2
data PhhhbbtttEither b a = Left a | Right b deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left x)  = Left (f x)
  fmap _ (Right x) = Right x

instance Applicative (PhhhbbtttEither b) where
  pure = Left
  (Left f)  <*> (Left x)  = Left (f x)
  (Right x) <*> _         = Right x
  _         <*> (Right x) = Right x

instance Monad (PhhhbbtttEither b) where
  return = pure
  (Left x)  >>= f = f x
  (Right x) >>= _ = Right x

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = frequency [(1, Right <$> arbitrary), (10, Left <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither a b) where
  (=-=) = eq

-- Exercise 3
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)

instance Monad Identity where
  return = pure
  (Identity x) >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-- Exercise 4
data List a = Nil | Cons a (List a) deriving (Eq, Show)

fromHList :: [a] -> List a
fromHList [] = Nil
fromHList (x:xs) = Cons x (fromHList xs)

take' :: Int -> List a -> List a
take' _ Nil         = Nil
take' n (Cons x xs)
  | n > 0     = Cons x (take' (n-1) xs)
  | otherwise = Nil

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure = flip Cons Nil
  Nil <*> _   = Nil
  _   <*> Nil = Nil
  fs  <*> xs  = go fs xs
    where go (Cons _ gs) Nil = go gs xs
          go Nil _ = Nil
          go l@(Cons f _) (Cons y ys) = Cons (f y) (go l ys)

instance Monad List where
  return = pure
  (>>=) m f = join $ fmap f m

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fromHList <$> arbitrary

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = take' 3000 xs
          ys' = take' 3000 ys

-- Write the following functions using the methods provided by
-- Monad and Functor. Using stuff like identity and composition is fine,
-- but it has to typecheck with types provided.
j :: Monad m => m (m a) -> m a
j = (>>= id)

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = f <$> ma <*> mb

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs f = foldr (\x acc -> f x >>= (\b -> fmap (b:) acc)) (pure []) xs

flipType :: (Monad m) => [m a] -> m [a]
flipType = (flip meh) id

main :: IO ()
main = do
  let tests x =
        concat
        [
          (unbatch $ functor x),
          (unbatch $ applicative x),
          (unbatch $ monad x)
        ]
      batch name x = quickBatch (name, tests x)
  batch
    "Nope"
    (NopeDotJpg :: Nope (Int, String, Bool))
  batch
    "PhhhbbtttEither"
    ((Left (1, "a", True)) :: PhhhbbtttEither Int (Int, String, Bool))
  batch
    "Identity"
    ((Identity (1, "a", True)) :: Identity (Int, String, Bool))
  batch
    "List"
    (Nil :: List (Int, String, Bool))
