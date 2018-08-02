module Main where

import Control.Monad (forever)
import Data.Char (toLower, isAlpha)
import System.Random

import Exercises.Hangman.Puzzle
import Exercises.Hangman.Game

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle

newtype WordList = WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "app/Hangman/data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList ((filter wordType . filter gameLength) aw)
  where gameLength w =
          let l = length (w :: String)
          in l >= minWordLength && l < maxWordLength
        wordType = all isAlpha


randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

