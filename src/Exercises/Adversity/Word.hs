module Exercises.Adversity.Word where

import Data.Char

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

isVowel :: Char -> Bool
isVowel c = elem c  vowels

isConsonant :: Char -> Bool
isConsonant c = isAlpha c && isVowel c == False

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

mkWord :: String -> Maybe Word'
mkWord str = if count isVowel str > count isConsonant str then Nothing else Just (Word' str)
