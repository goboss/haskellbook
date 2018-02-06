module Strings where

import Data.List

-- Exercise 1
-- Write a recursive function named replaceThe which takes a text/string,
-- breaks it into words and replaces each instance of “the” with “a”.
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe  str  = Just str

replaceThe :: String -> String
replaceThe = concat . intersperse " " . map (toString . notThe) . words
  where toString Nothing  = "a"
        toString (Just str) = str

-- Exercise 2
-- Write a recursive function that takes a text/string, breaks it into words,
-- and counts the number of instances of ”the”
-- followed by a vowel-initial word.
isVowel :: Char -> Bool
isVowel c = elem c "aiueo" -- japanese order of vowels :)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = cnt 0 [] . words
  where cnt acc _ [] = acc
        cnt acc prev (w:ws) = if prev == "the" && vowelInitial w then cnt (acc+1) w ws else cnt acc w ws
        vowelInitial ""     = False
        vowelInitial (x:_) = isVowel x

-- Exercise 3
-- Return the number of letters that are vowels in a word.
countVowels :: String -> Integer
countVowels = fromIntegral . length . filter isVowel

