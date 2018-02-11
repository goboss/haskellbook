module Exercises.Monoid.Optional where

import Exercises.Monoid.Laws
import Test.QuickCheck

-- Exercise
-- Write the Monoid instance for our Maybe type renamed to Optional.
data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Only (mempty)

  mappend x Nada  = x
  mappend Nada x = x
  mappend (Only x) (Only y) = Only (mappend x y)

-- Exercise
-- Write a Monoid instance for a Maybe type which doesn’t require a Monoid
-- for the contents.
newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend f@(First' (Only _)) _ = f
  mappend _ other = other

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency [(10, return (First' (Only a))),
               (1, return (First' Nada))]

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId = First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
