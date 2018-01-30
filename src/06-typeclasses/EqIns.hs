module EqIns where

-- Exercise 1

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  TisAn x == TisAn y = x == y

-- Exercise 2

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  Two a b == Two c d = a == c && b == d

-- Exercise 3

data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  TisAnInt x == TisAnInt y = x == y
  TisAString x == TisAString y = x == y
  _ == _ = False

-- Exercise 4

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  Pair x y == Pair x' y' = x == x' && y == y'

-- Exercise 5

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  Tuple a b == Tuple a' b' = a == a' && b == b'

-- Exercise 6

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  ThisOne a == ThisOne a' = a == a'
  ThatOne a == ThatOne a' = a == a'
  _ == _ = False


-- Exercise 7

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  Hello a == Hello a' = a == a'
  Goodbye b == Goodbye b' = b == b'
  _ == _ = False

