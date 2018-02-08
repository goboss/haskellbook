module Exercises.Algebraic.Language where

import Data.Char

-- Exercise 1
-- Write a function that capitalizes a word.
capitalizeWord :: String -> String
capitalizeWord ""     = ""
capitalizeWord (x:xs) =  toUpper x : xs

-- Exercise 2
-- Write a function that capitalizes sentences in a paragraph.
-- Recognize when a new sentence has begun by checking for periods.
-- Reuse the capitalizeWord function.
capitalizeParagraph :: String -> String
capitalizeParagraph = concat . map capitalizeSpaceWord . paragraphs
  where capitalizeSpaceWord s =
          takeWhile isSpace s ++ capitalizeWord (dropWhile isSpace s)

paragraphs :: String -> [String]
paragraphs str =
  let
    step c (acc, tmp) = if c == '.' then (tmp : acc, ".") else (acc, c : tmp)
    (parag, rest) = foldr step ([], "") str
  in
    rest : parag
