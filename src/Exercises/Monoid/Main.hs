module Exercises.Monoid.Main where

import Data.Semigroup
import Exercises.Monoid.Laws
import Test.QuickCheck hiding (Failure, Success)

-- Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- Identity
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

-- Two
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

-- Three
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c)
       => Semigroup (Three a b c) where
  (Three x y z) <> (Three x' y' z') = Three (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c)
       => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool

-- Four
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
       => Semigroup (Four a b c d) where
  (Four x y z v) <> (Four x' y' z' v') =
    Four (x <> x') (y <> y') (z <> z') (v <> v')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
       => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

type FourAssoc a b c d = Four a b c d -> Four a b c d -> Four a b c d -> Bool

-- BoolConj
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj x) <> (BoolConj y) = BoolConj (x && y)

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = fmap BoolConj (arbitrary :: Gen Bool)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- Bool Disj
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj x) <> (BoolDisj y) = BoolDisj (x || y)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance Arbitrary BoolDisj where
  arbitrary = fmap BoolDisj (arbitrary :: Gen Bool)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- Or
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  (Snd x) <> _ = Snd x
  _ <> other   = other

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    fs <- arbitrary :: Gen Bool
    return $ if fs then Fst a else Snd b

type OrAssoc a b = Or a b ->  Or a b -> Or a b -> Bool

-- Combine
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\x -> f x <> g x)

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend = (<>)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return (Combine f)

-- Exercise 10
newtype Comp a = Comp (a -> a)

instance Semigroup a => Semigroup (Comp a) where
  (Comp x) <> (Comp y) = Comp (y . x)

instance (Semigroup a, Monoid a) => Monoid (Comp a ) where
  mempty = Comp mempty
  mappend = (<>)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary
    return (Comp f)

-- Validation
-- the behaviour proposed in the book is really wierd, so I'm implementing
-- something more useful
data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Success _) <> (Success x) = Success x
  (Failure x) <> (Failure y) = Failure (x <> y)
  (Failure x) <> _           = Failure x
  _           <> (Failure y) = Failure y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(10, return (Success b)), (1, return (Failure a))]

type ValidationAssoc a b =
  Validation a b -> Validation a b -> Validation a b -> Bool

-- Mem
newtype Mem s a =
  Mem {
    runMem :: s -> (a,s)
  }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s))
  mappend (Mem f) (Mem g) =
    Mem (\s ->
      let
        (a1, s1) = f s
        (a2, s2) = g s1
      in
        (mappend a1 a2, s2)
    )

main :: IO ()
main = do
  quickCheck
    (semigroupAssoc :: TrivAssoc)
  quickCheck
    (monoidAssoc :: TrivAssoc)
  quickCheck
    (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck
    (monoidRightIdentity :: Trivial -> Bool)
  quickCheck
    (semigroupAssoc :: (IdentityAssoc (Sum Int)))
  quickCheck
    (monoidAssoc :: (IdentityAssoc (Sum Int)))
  quickCheck
    (monoidLeftIdentity :: Identity Any -> Bool)
  quickCheck
    (monoidRightIdentity :: Identity Any -> Bool)
  quickCheck
    (semigroupAssoc :: (TwoAssoc (Sum Int) (Product Int)))
  quickCheck
    (monoidAssoc :: (TwoAssoc (Sum Int) (Product Int)))
  quickCheck
    (monoidLeftIdentity :: Two (Sum Int) (Product Int) -> Bool)
  quickCheck
    (monoidRightIdentity :: Two (Sum Int) (Product Int) -> Bool)
  quickCheck
    (semigroupAssoc :: (ThreeAssoc (Sum Int) (Product Int) Any))
  quickCheck
    (semigroupAssoc :: (FourAssoc (Sum Int) (Product Int) Any All))
  quickCheck
    (semigroupAssoc :: BoolConjAssoc)
  quickCheck
    (monoidAssoc :: BoolConjAssoc)
  quickCheck
    (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck
    (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck
    (semigroupAssoc :: BoolDisjAssoc)
  quickCheck
    (monoidAssoc :: BoolDisjAssoc)
  quickCheck
    (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck
    (monoidRightIdentity :: BoolDisj -> Bool)
  quickCheck
    (semigroupAssoc :: OrAssoc Any All)
  quickCheck
    (semigroupAssoc :: ValidationAssoc Any All)
