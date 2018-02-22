module Exercises.Traversable.Main where

import Data.Monoid

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Write a Traversable instance for the datatype provided, filling in any
-- required superclasses. Use QuickCheck to validate your instances.

-- Exercise 1
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-- Exercise 2
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant c) = Constant c

instance Foldable (Constant a) where
  foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure (Constant a)

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (Constant x) =-= (Constant y) = eq x y

-- Exercise 3
data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
  fmap f (Yep x) = Yep (f x)
  fmap _ _       = Nada

instance Foldable Optional where
  foldMap f (Yep x) = f x
  foldMap _ _       = mempty

instance Traversable Optional where
  traverse f (Yep x) = Yep <$> f x
  traverse _ _       = pure Nada

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, return Nada), (10, Yep <$> arbitrary)]

instance Eq a => EqProp (Optional a) where
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
  fmap f (Cons h t) = Cons (f h) (fmap f t)
  fmap _ _          = Nil

instance Foldable List where
  foldMap f (Cons h t) = mappend (f h) (foldMap f t)
  foldMap _ _          = mempty

instance Traversable List where
  traverse f (Cons h t) = Cons <$> (f h) <*> traverse f t
  traverse _ _          = pure Nil

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fmap fromHList arbitrary

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = take' 3000 xs
          ys' = take' 3000 ys

-- Exercise 5
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three a b) where
  foldMap f (Three _ _ z) = f z

instance Traversable (Three a b) where
  traverse f (Three x y z) = (Three x y) <$> f z

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- Exercise 6
data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y)

instance Foldable (Pair a) where
  foldMap f (Pair _ y) = f y

instance Traversable (Pair a) where
  traverse f (Pair x y) = Pair x <$> f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

-- Exercise 7
data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big x y z) = Big x (f y) (f z)

instance Foldable (Big a) where
  foldMap f (Big _ x y) = mappend (f x) (f y)

instance Traversable (Big a) where
  traverse f (Big x y z) = Big x <$> f y <*> f z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

-- Exercise 8
data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b c d) = Bigger a (f b) (f c) (f d)

instance Foldable (Bigger a) where
  foldMap f (Bigger _ a b c) = f a <> f b <> f c

instance Traversable (Bigger a) where
  traverse f (Bigger a b c d) = Bigger a <$> f b <*> f c <*> f d

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

-- Exercise 9
data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S n a) = S (fmap f n) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S n a) = foldMap f n <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S n a) = S <$> traverse f n <*> f a

instance ( Functor n
         , Arbitrary (n a)
         , Arbitrary a)
        => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq (n a), Eq a) => EqProp (S n a) where
  (=-=) = eq

-- Exercise 10
data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty        = Empty
  fmap f (Leaf x)     = Leaf (f x)
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
  foldMap _ Empty        = mempty
  foldMap f (Leaf x)     = f x
  foldMap f (Node l x r) = (foldMap f l) <> (f x) <> (foldMap f r)

instance Traversable Tree where
  traverse _ Empty        = pure Empty
  traverse f (Leaf x)     = Leaf <$> f x
  traverse f (Node l x r) = Node <$> traverse f l <*> f x <*> traverse f r

genTree :: Arbitrary a => Gen (Tree a)
genTree = do
  x <- arbitrary
  l <- genTree
  r <- genTree
  frequency
    [
      (1, return Empty),
      (2, return $ Leaf x),
      (3, return $ Node l x r)
    ]

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = genTree

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

type S3 = (String, String, String)

main :: IO ()
main = do
  let tests x =
        concat
        [
          (unbatch $ functor x),
          (unbatch $ traversable x)
        ]
      batch name x = quickBatch (name, tests x)
  batch "Identity" (undefined :: Identity S3)
  batch "Constant" (undefined :: Constant Int S3)
  batch "Optional" (undefined :: Optional S3)
  batch "List"     (undefined :: List S3)
  batch "Three"    (undefined :: Three String String S3)
  batch "Pair"     (undefined :: Pair String S3)
  batch "Big"      (undefined :: Big String S3)
  batch "Bigger"   (undefined :: Bigger String S3)
  batch "S"        (undefined :: S [] S3)
  batch "Tree"     (undefined :: Tree S3)
