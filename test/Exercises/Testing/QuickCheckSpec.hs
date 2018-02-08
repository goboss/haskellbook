{-# LANGUAGE FlexibleInstances #-}

module Exercises.Testing.QuickCheckSpec where

import Test.Hspec
import Test.QuickCheck

import Data.List (sort)


spec :: Spec
spec = do
  -- Exercise 1
  describe "half" $ do
    it "returns identity for half * 2" $ property $
      \x -> ((*2) . half) x == (x :: Double)
  -- Exercise 2
  describe "sort" $ do
    it "orders list of Int" $ property $
      \xs -> listOrdered (sort xs :: [Int])
  -- Exercise 3
  describe "addition" $ do
    it "is associative" $ property $
      \x y z -> x + (y + z) == (x + y) + (z :: Int)
    it "is commutative" $ property $
      \x y -> x + y == y + (x :: Int)
  -- Exercise 4
  describe "multiplication" $ do
    it "is associative" $ property $
      \x y z -> x * (y * z) == (x * y) * (z :: Int)
    it "is commutative" $ property $
      \x y -> x * y == y * (x :: Int)
  -- Exercise 5
  describe "integral division" $ do
    it "calculates quotient and remainder correctly" $ property $
      \x (NonZero y) -> (quot x y) * y + (rem x y) == (x :: Int)
    it "calculates divisor and modulus correctly" $ property $
      \x (NonZero y) -> (div x y)*y + (mod x y) == (x :: Int)
  -- Exercise 7
  describe "reverse" $ do
    it "is idenity when applied twice" $ property $
      \x -> (reverse . reverse) x == id (x :: [Int])
  -- Exercise 8
  describe "function operators" $ do
    it "applies argument to a function" $ property $
      \x f -> (f $ x) == (f :: Int -> Int) x
    it "combines functions" $ property $
      \f g x -> ((f :: Int -> Int) . (g :: Int -> Int)) x == f (g x)
  -- Exercise 9
  describe "foldr" $ do
    it "concatenates lists" $ property $
      \x y -> foldr (:) y x == x ++ (y :: [Int])
    it "joins list" $ property $
      \x -> foldr (++) [] x == concat (x :: [[Int]])
  -- Exercise 11
  describe"show and read" $ do
    it "reads what was shown" $ property $
      \x -> read (show x) == (x :: Int)

half :: Fractional a => a -> a
half x = x / 2

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)

instance Show (Int -> Int) where
  show _ = "Int -> Int"
