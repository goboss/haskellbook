module Recursion where

-- Exercise 1
-- Write a function that recursively sums all numbers from 1 to n.
sumi :: (Eq a, Num a) => a -> a
sumi x = if (x == abs x) then go x 0 else 0
  where go 0 acc = acc
        go n acc = go (n - 1) (acc + n)

-- Exercise 2
-- Write a function that multiplies two integral numbers
-- using recursive summation.
mul :: (Integral a) => a -> a -> a
mul a b = (signum a) * go (abs a) b 0
  where go _ 0 acc = acc
        go 0 _ acc = acc
        go x y acc = go (x - 1) y (acc + y)

-- Fixing divideBy
data DividedResult = Result (Integer, Integer) | DividedByZero deriving Show

dividedBy :: Integer -> Integer -> DividedResult
dividedBy num denom = go (abs num) (abs denom) 0
  where go n d count
          | d == 0 = DividedByZero
          | n < d && n /= 0 && signum num == negate (signum denom) = Result (signum denom * signum num * count - 1, signum denom * n + signum denom)
          | n < d = Result (signum denom * signum num * count, signum denom * n)
          | otherwise = go (n - d) d (count + 1)

-- McCarthy 91
mc91 :: Integral a => a -> a
mc91 x
  | x > 100   = x - 10
  | otherwise = mc91 . mc91 $ x + 11

