module Exercises.Applicative.Validation where

import Data.Monoid
import Test.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
  fmap f (Success x) = Success (f x)
  fmap _ (Failure x) = Failure x

-- This is different
instance Monoid e => Applicative (Validation e) where
  pure = Success
  (Failure e) <*> (Success _) = Failure e
  (Success _) <*> (Failure e) = Failure e
  (Failure e) <*> (Failure x) = Failure (e <> x)
  (Success f) <*> (Success x) = Success (f x)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary =
    frequency [(1, Failure <$> arbitrary), (10, Success <$> arbitrary)]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

main :: IO ()
main = quickBatch $
  applicative ((Success (1, 'a', True)) :: Validation String (Int, Char, Bool))
