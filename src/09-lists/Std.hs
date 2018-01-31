module Std where

-- Exercise 1
-- myOr returns True if any Bool in the list is True.
myOr :: [Bool] -> Bool
myOr [] = False
myOr (b:bs) = b || myOr bs

-- Exercise 2
-- myAny returns True if a -> Bool applied to any of the values 
-- in the list returns True.
myAny :: (a -> Bool) -> [a] -> Bool
myAny f as = myOr $ map f as

-- Exercise 3
-- After you write the recursive myElem, write another version that uses any.
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = x == a || myElem a xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' a as = myAny (== a) as

-- Exercise 4
-- Implement myReverse.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = accum xs []
  where accum [] acc     = acc
        accum (y:ys) acc = accum ys (y : acc) 

-- Exercise 5
-- squish flattens a list of lists into a list.
squish :: [[a]] -> [a]
squish [] = []
squish (l:ls) = l ++ squish ls

-- Exercise 6
-- squishMap maps a function over a list and concatenates the results.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f as = squish $ map f as

-- Exercise 7
-- squishAgain flattens a list of lists into a list. This time re-use the squishMap function.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- Exercise 8
-- myMaximumBy takes a comparison function and a list and returns the greatest element 
-- of the list based on the last value that the comparison returned GT for.
myBetterBy :: Ordering -> (a -> a -> Ordering) -> [a] -> a
myBetterBy _ _ [] = error "empty list"
myBetterBy ord cmp (a : as) = better as a
  where better [] cur = cur
        better (x : xs) cur = better xs (if (cmp x cur == ord) then x else cur)

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = myBetterBy GT

-- Exercise 9
-- myMinimumBy takes a comparison function and a list and returns the least element 
-- of the list based on the last value that the comparison returned LT for.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = myBetterBy LT

-- Exercise 10
-- Using the myMinimumBy and myMaximumBy functions, write your own 
-- versions of maximum and minimum.
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

