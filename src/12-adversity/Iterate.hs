module Iterate where

-- Exercise 1
-- Write the function myIterate using direct recursion.
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

-- Exercise 2
-- Write the function myUnfoldr using direct recursion.
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x =
  case f x of
    Nothing      -> []
    Just (a, b)  -> a : myUnfoldr f b

-- Exercise 3
-- Rewrite myIterate into betterIterate using myUnfoldr.
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\x -> Just (x, f x)) x

