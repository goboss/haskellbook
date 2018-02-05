module Phone where

import Data.Char
import Data.List

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

-- Exercise 1
-- Create a data structure that captures the phone layout above.

data ButtonMapping = Lit Char | Uppercase

data PhoneButton = Button { label :: Digit, mappings :: [ButtonMapping] }

data DaPhone = Phone [PhoneButton]

mappingsFromStr :: String -> [ButtonMapping]
mappingsFromStr = map Lit

myPhone :: DaPhone
myPhone =
  Phone [
    (Button '1' (mappingsFromStr "1")),
    (Button '2' (mappingsFromStr "abc2")),
    (Button '3' (mappingsFromStr "def3")),
    (Button '4' (mappingsFromStr "ghi4")),
    (Button '5' (mappingsFromStr "jkl5")),
    (Button '6' (mappingsFromStr "mno6")),
    (Button '7' (mappingsFromStr "pqrs7")),
    (Button '8' (mappingsFromStr "tuv8")),
    (Button '9' (mappingsFromStr "wxyz9")),
    (Button '*' (Uppercase : mappingsFromStr "*")),
    (Button '0' (mappingsFromStr " +_0")),
    (Button '#' (mappingsFromStr ".,#"))
  ]

buildIndex :: DaPhone -> [(Char, [(Digit, Presses)])]
buildIndex (Phone buttons) = foldr step [] buttons
  where step b acc = (index (label b) (mappings b) 1) ++ acc
        index _ [] _ = []
        index d ((Lit c) : xs) i =
          (c, [(d, i)]) : (toUpper c, [('*', 1), (d, i)]) : index d xs (i+1)
        index d (_ : xs) i = index d xs (i+1)

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Just making sure rofl ur turn"]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone c =
  let
    index = buildIndex phone
    lookup c ((x, map) : xs) = if c == x then map else lookup c xs
    lookup c [] = []
  in
    lookup c index

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concat . map (reverseTaps phone)

-- Exercise 3
-- How many times do digits need to be pressed for each message?
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

-- Exercise 4
-- What was the most popular letter for each message? What was its cost?
compressSubseq :: (Eq a) => [a] -> [(Int, a)]
compressSubseq = cnt []
  where cnt acc [] = acc
        cnt acc@((count, c):ys) (x:xs) =
          if x == c
          then
            cnt ((count + 1,  c) : ys) xs
          else
            cnt ((1, x) : acc) xs
        cnt [] (x:xs) = cnt [(1, x)] xs

mostPopularLetter :: String -> Char
mostPopularLetter = snd . maximum . compressSubseq . sort

costOfFame :: DaPhone -> String -> Presses
costOfFame phone = maximum . map (\(cnt, c) -> cnt * fingerTaps (reverseTaps phone c)) . compressSubseq . sort

-- Exercise 5
-- What was the most popular letter overall? What was the most popular word?
coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = snd . maximum . compressSubseq . sort . concat . map words
