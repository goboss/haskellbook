module Exercises.Folds.Warmup where

-- Exercise 1a
-- Write a function that takes inputs from stops and vowels and makes
-- 3-tuples of all possible stop-vowel-stop combinations.
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

stopVowelStops :: [(Char, Char, Char)]
stopVowelStops = [(s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops]

-- Exercise 1b
-- Modify that function so that it only returns the combinations
-- that begin with a p.
stopVowelStops' :: [(Char, Char, Char)]
stopVowelStops' = [(s1, v, s2) | s1 <- "p", v <- vowels, s2 <- stops]

-- Exercise 1c
-- Now set up lists of nouns and verbs (instead of stops and vowels)
-- and modify the function to make tuples representing possible
-- noun-verb-noun sentences.
nouns :: [String]
nouns = ["kitteh", "dog", "Julie", "Chris", "cupcake", "lambda", "spaceship"]

verbs :: [String]
verbs = ["licks", "likes", "calculates", "frowns at", "adopts", "builds"]

sentences :: [(String, String, String)]
sentences = [(n1, v, n2) | n1 <- nouns, v <- verbs, n2 <- nouns]

-- Exercise 3
-- Rewrite seekritFunc to use fractional division
seekritFunc :: String -> Double
seekritFunc x = fromIntegral (sum (map length (words x))) / fromIntegral ((length (words (x))))
