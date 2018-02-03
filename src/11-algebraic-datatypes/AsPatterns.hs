module AsPatterns where

import Data.Char

-- Use as-patterns in implementing the following functions.

-- Exercise 1
-- This should return True if (and only if) all the values in the first list
-- appear in the second list, though they need not be contiguous.
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf sub@(x:xs) (y:ys)
  | x == y    = isSubseqOf xs ys
  | otherwise = isSubseqOf sub ys

-- Exercise 2
-- Split a sentence into words, then tuple each word with the capitalized
-- form of each.
capitalizeWords :: String -> [(String, String)]
capitalizeWords = map capitalize . words
  where capitalize word@(c:cs) = (word, toUpper c : cs)

