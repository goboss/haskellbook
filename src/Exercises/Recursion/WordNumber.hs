module Exercises.Recursion.WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = names !! n
  where names = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"]

digits :: Int -> [Int]
digits n = go (abs n) []
  where go 0 [] = [0]
        go 0 acc = acc
        go x acc = go (x `div` 10) (x `mod` 10 : acc)

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
