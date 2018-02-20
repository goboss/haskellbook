module Exercises.Foldable.Library where

import Data.Monoid

-- Implement the functions in terms of foldMap or foldr from Foldable,
-- then try them out with multiple types that have Foldable instances.

-- Exercise 1
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

-- Exercise 2
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

-- Exercise 3
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (\y -> Any(y == x))

-- Exercise 4
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr min' Nothing
  where min' x Nothing = Just x
        min' x y       = min (Just x) y

-- Exercise 5
newtype Max a = Max { getMax :: Maybe a }
instance Ord a => Monoid (Max a) where
  mempty = Max Nothing
  mappend x (Max Nothing) = x
  mappend (Max x) (Max y) = Max (max x y)

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = getMax . foldMap (\x -> Max(Just x))

-- Exercise 6
null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

-- Exercise 7
length' :: (Foldable t) => t a -> Int
length' = getSum . foldMap (\_ -> Sum 1)

-- Exercise 8
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

-- Exercise 9
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-- Exercise 10
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty
