module Exercises.Applicative.Main where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Write instances for the following datatypes.

-- Exercise 1
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

-- Exercise 2
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (Two a f) <*> (Two b x) = Two (a <> b) (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

-- Exercise 3
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (Three a b f) <*> (Three c d x) = Three (a <> c) (b <> d) (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c)
       => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- Exercise 4
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (Three' a f g) <*> (Three' b x y) = Three' (a <> b) (f x) (g y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

-- Exercise 5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (Four a b c f) <*> (Four a' b' c' x) =
    Four (a <> a') (b <> b') (c <> c') (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
       => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

-- Exercise 6
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance Monoid a => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (Four' a b c f) <*> (Four' a' b' c' x) =
    Four' (a <> a') (b <> b') (c <> c') (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch
    (
      "Pair",
      unbatch $ applicative ((Pair (1, 'a', True) (2, 'z', False)) :: Pair (Int, Char, Bool))
    )
  quickBatch
    (
      "Two",
      unbatch $ applicative ((Two "test" (False, 'z', 2)) :: Two String (Bool, Char, Int))
    )
  quickBatch
    (
      "Three",
      unbatch $ applicative ((Three "test" [1,2] (1, 'a', True) :: Three String [Int] (Int, Char, Bool)))
    )
  quickBatch
    (
      "Three'",
      unbatch $ applicative ((Three' "test" (1, 'a', True) (2, 'b', False) :: Three' String (Int, Char, Bool)))
    )
  quickBatch
    (
      "Four",
      unbatch $ applicative ((Four "a" "b" "c" (2, 'b', False) :: Four String String String (Int, Char, Bool)))
    )
  quickBatch
    (
      "Four'",
      unbatch $ applicative ((Four' "a" "b" "c" (2, 'b', False) :: Four' String (Int, Char, Bool)))
    )
