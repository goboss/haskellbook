module Folds where

-- Exercise 1
-- myOr returns True if any Bool in the list is True.
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- Exercise 2
-- myAny returns True if a -> Bool applied to any of the values in the
-- list returns True.
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

-- Exercise 3
-- Write two versions of myElem. One version should use folding
-- and the other should use any.
myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr ((||) . (==x)) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' = myAny . (==)

-- Exercise 4
-- Implement myReverse, don’t worry about trying to make it lazy.
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- Exercise 5
-- Write myMap in terms of foldr. It should have the same behavior
-- as the built-in map.
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

-- Exercise 6
-- Write myFilter in terms of foldr. It should have the same behavior
-- as the built-in filter.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a as -> if (f a) then a : as else as) []

-- Exercise 7
-- squish flattens a list of lists into a list
squish :: [[a]] -> [a]
squish = foldr (++) []

-- Exercise 8
-- squishMap maps a function over a list and concatenates the results.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

-- Exercise 9
-- squishAgain flattens a list of lists into a list. This time reuse the
-- squishMap function.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- Exercise 10
-- myMaximumBy takes a comparison function and a list and returns
-- the greatest element of the list based on the last value that the
-- comparison returned GT for.
myBetterBy :: Ordering -> (a -> a -> Ordering) -> [a] -> a
myBetterBy ord cmp as = foldl (\a acc -> if (cmp a acc == ord) then a else acc) (head as) as

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = myBetterBy GT

-- Exercise 11
-- myMinimumBy takes a comparison function and a list and returns
-- the least element of the list based on the last value that the
-- comparison returned LT for.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = myBetterBy LT

