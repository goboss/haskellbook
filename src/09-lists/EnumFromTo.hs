module EnumFromTo where

-- Exercise: Write your own enumFromTo definitions for the types provided. 
-- Do not use range syntax to do so.
-- (I'm assuming enumFromTo is also cheting :))

eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool False True  = [False, True]
eftBool True True  = [True]
eftBool _ _ = []

eftOrdEnum :: (Enum a, Ord a) => a -> a -> [a]
eftOrdEnum from to = go from to []
  where go x y acc
          | x > y     = reverse acc
          | x == y    = reverse (x : acc)
          | otherwise = go (succ x) y (x : acc) 

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eftOrdEnum

eftInt :: Int -> Int -> [Int]
eftInt = eftOrdEnum

eftChar :: Char -> Char -> [Char]
eftChar = eftOrdEnum
