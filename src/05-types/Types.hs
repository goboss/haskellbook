module Types where

-- Write a type signature

functionH :: [a] -> a
functionH (x:_) = x

functionC :: (Ord a) => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

-- Given a type, write the function

i :: a -> a
i x = x

c :: a -> b -> a
c x _ = x

c'' :: b -> a -> b
c'' x _ = x

c' :: a -> b -> b
c' _ y =  y

r :: [a] -> [a]
r = take 1 -- or drop 2 or id or ...

co :: (b -> c) -> (a -> b) -> a -> c
co f g a =  f (g a)

a :: (a -> c) -> a -> a
a _ x = x

a' :: (a -> b) -> a -> b
a' f a = f a
