module Exercises.Typeclasses.TypeKwonDo where

-- Exercise 1

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b

-- Exercise 2

arith :: Num b
      => (a -> b)
      -> Integer
      -> a
      -> b
arith f i a = f a + (fromInteger i)
