module Exercises.Testing.IdempotenceSpec where

import Test.Hspec
import Test.QuickCheck

import Exercises.Algebraic.Language (capitalizeWord)
import Data.List (sort)

spec :: Spec
spec = do
  -- Exercise 1
  describe "capitalizeWord" $ do
    it "is idempotent" $ property $
      \x -> (capitalizeWord x == twice capitalizeWord x) &&
              (capitalizeWord x == fourTimes capitalizeWord x)
  -- Exercise 2
  describe "sort" $ do
    it "is idempotent" $ property $
      \x -> (sort x == twice sort x) &&
              (sort x == fourTimes sort (x :: [Int]))

twice :: (a -> a) -> (a -> a)
twice f = f . f

fourTimes :: (a -> a) -> (a -> a)
fourTimes = twice . twice
