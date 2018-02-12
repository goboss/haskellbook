module Exercises.Functor.Instances where

import Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

-- Exercise 1
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

-- Exercise 2
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

-- Exercise 3
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

-- Exercise 4
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c)
       => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

-- Exercise 5
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three' a b c)

-- Exercise 6
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
       => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

-- Exercise 7
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four' a b c d)

-- Exercise 8
-- No I can't make Functor instance from Trivial - it's of kind *

-- Exercise
-- Write a Functor instance for a datatype identical to Maybe.
data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap f (Yeppers x) = Yeppers (f x)
  fmap _ LolNope     = LolNope

instance Arbitrary a => Arbitrary (Possibly a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return LolNope), (10, return (Yeppers a))]

-- Exercise
-- Write a Functor instance for a datatype identical to Either.
data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second b) = Second (f b)
  fmap _ (First a)  = First a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ First a), (10, return $ Second b)]

type Int2Int = Fun Int Int
type Int2Str = Fun Int String
type Str2Int = Fun String Int

main :: IO ()
main = do
  quickCheck
    (functorIdentity :: Identity Int -> Bool)
  quickCheck
    (functorCompose' :: Identity Int -> Int2Int -> Int2Int -> Bool)
  quickCheck
    (functorIdentity :: Pair Int -> Bool)
  quickCheck
    (functorCompose' :: Pair Int -> Int2Str -> Str2Int -> Bool)
  quickCheck
    (functorIdentity :: Two Int String -> Bool)
  quickCheck
    (functorCompose' :: Two Int Int -> Int2Str -> Str2Int -> Bool)
  quickCheck
    (functorIdentity :: Three Int Bool String -> Bool)
  quickCheck
    (functorCompose' :: Three Int Bool Int -> Int2Str -> Str2Int -> Bool)
  quickCheck
    (functorIdentity :: Three' Int String -> Bool)
  quickCheck
    (functorCompose' :: Three' Int Int -> Int2Str -> Str2Int -> Bool)
  quickCheck
    (functorIdentity :: Four Int Int Int Int -> Bool)
  quickCheck
    (functorCompose' :: Four Int Int Int Int -> Int2Str -> Str2Int -> Bool)
  quickCheck
    (functorIdentity :: Four' Int String -> Bool)
  quickCheck
    (functorCompose' :: Four' Int Int -> Int2Str -> Str2Int -> Bool)
  quickCheck
    (functorIdentity :: Possibly Int -> Bool)
  quickCheck
    (functorCompose' :: Possibly Int -> Int2Str -> Str2Int -> Bool)
  quickCheck
    (functorIdentity :: Sum String Int -> Bool)
  quickCheck
    (functorCompose' :: Sum String Int -> Int2Str -> Str2Int -> Bool)
