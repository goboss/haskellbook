module Exercises.Hangman.Game where

import Control.Monad (forever)
import Data.Maybe (isJust, isNothing)
import System.Exit (exitSuccess)

import Exercises.Hangman.Puzzle

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess filledInSoFar _ attempts) =
  if attempts == 0 && any isNothing filledInSoFar then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle word filledInSoFar _ _) =
  if all isJust filledInSoFar then
    do putStrLn $ "You correctly guessed \"" ++ word ++ "\". Victory!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must\
                   \ be a single character"

