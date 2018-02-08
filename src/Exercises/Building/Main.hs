module Exercises.Building.Main where

import Data.Char (toLower, isLetter)
import Control.Monad
import System.Exit (exitSuccess)

-- Exercise 2, 3
-- Modify it to exit successfully after a False result.
-- If you tried using palindrome on a sentence such as “Madam I’m Adam,”
-- you may have noticed that palindrome checker doesn’t work on that.
palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let cleaned = clean line1
  case (cleaned == reverse cleaned) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess
  where clean = filter isLetter . map toLower

-- Exercise 4
-- Your job is to write the following function without modifying the code above.
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
      "Name was: " ++ show name ++
      " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Enter name:"
  name <- getLine
  putStrLn "Enter age:"
  age <- getLine
  case (mkPerson name (read age :: Integer)) of
    Right p@(Person _ _) -> putStrLn $ "Yay! Successfully got a person: " ++ show p
    Left NameEmpty -> putStrLn "Name was empty!"
    Left AgeTooLow  -> putStrLn "That person is a bit too young!"
    Left (PersonInvalidUnknown s) -> putStrLn s
