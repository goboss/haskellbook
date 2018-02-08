module Exercises.Strings.Main where

-- Exercise 2: Reimplment as named functions
emphasise :: String -> String
emphasise s = s ++ "!"

strAt :: String -> Int -> String
strAt s n = take 1 $ drop n s

drop9 :: String -> String
drop9 s = drop 9 s

-- Exercise 3: Write a function of type String -> Char
-- which returns the third character in a String.

thirdLetter :: String -> Char
thirdLetter s = s !! 2

-- Exercise 4: Change the above function so the string operated on is always the
-- same and the variable represents the number of the letter you want to return
letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! (x - 1)

-- Exercise 5: Using take and drop take the string “Curry is awesome” and
-- return the result “awesome is Curry.”
rvrs :: String -> String
rvrs s = fst ++ snd ++ trd ++ "."
  where fst = drop 9 s
        snd = take 4 (drop 5 s)
        trd = take 5 s
