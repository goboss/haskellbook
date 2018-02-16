module Exercises.Applicative.List where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Exercise
-- Implement the list Applicative.
data List a = Nil | Cons a (List a) deriving (Eq, Show)

fromHList :: [a] -> List a
fromHList [] = Nil
fromHList (x:xs) = Cons x (fromHList xs)

take' :: Int -> List a -> List a
take' _ Nil         = Nil
take' n (Cons x xs)
  | n > 0     = Cons x (take' (n-1) xs)
  | otherwise = Nil

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' _ Nil _ = Nil
zipWith' _ _ Nil = Nil
zipWith' f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith' f xs ys)

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

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fmap fromHList arbitrary

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = take' 3000 xs
          ys' = take' 3000 ys

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs in take' 3000 l
          ys' = let (ZipList' l) = ys in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure = ZipList' . repeat'
  _              <*> (ZipList' Nil) = ZipList' Nil
  (ZipList' Nil) <*> _              = ZipList' Nil
  (ZipList' xs)  <*> (ZipList' ys)  = ZipList' $ zipWith' ($) xs ys

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = fmap ZipList' arbitrary

main :: IO ()
main = do
  quickBatch $ applicative (Nil :: List (Int, Int, String))
  quickBatch $ applicative (ZipList' Nil :: ZipList' (Int, Int, String))
