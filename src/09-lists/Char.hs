module Char where

import Data.Char

-- Exercise 2: Write a function that filters all the uppercase letters
-- out of a String.
onlyUpper :: String -> String
onlyUpper = filter isUpper

-- Exercise 3: Write a function that will capitalize the first letter
-- of a string and return the entire string.
capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c : cs

-- Exercise 4: Now make a new version of that function that is recursive
-- such that if you give it the input “woot” it will holler back at you "WOOT".
capitalizeAll :: String -> String
capitalizeAll = map toUpper

-- Exercise 5: Now write a function that will capitalize the first letter of 
-- a String and return only that letter as the result.
-- Exercise 6: Cool. Good work. Now rewrite it as a composed function.
-- Then, for fun, rewrite it pointfree.
fstCapitalized :: String -> Char
fstCapitalized = head . capitalize

