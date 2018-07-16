{-# LANGUAGE LambdaCase #-}

module Exercises.Transformers.Morra where

import qualified Data.Map as M
import           Data.List (intercalate)
import           Data.Foldable (maximumBy)
import           Data.Traversable (sequence)
import           Data.Maybe (maybe)
import           Control.Monad (mfilter, when, replicateM_)
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class
import           System.Random (getStdRandom, randomR)
import           System.Console.Terminal.Size (size, height)
import           Text.Read (readMaybe)

data Player = Human String | CPU deriving Eq

data Hand = Zero | One | Two | Three | Four | Five deriving (Eq, Show)
type Guess = Int
type Score = Int

type Move = (Hand, Guess)

type Turn = M.Map Player Move

data PlayerState = PlayerState
  { getScore :: Score
  , getHistory :: [Hand]
  }

type GameState = M.Map Player PlayerState

data Mode = Singleplayer | Multiplayer deriving Eq

instance Show Player where
  show CPU = "#CPU"
  show (Human name) = name

instance Ord Player where
  compare (Human p1) (Human p2) = compare p1 p2
  compare _ CPU                 = LT
  compare CPU _                 = GT

insist :: String -> IO (Maybe a) -> IO a
insist msg io =
  io >>= \case
    Just a -> return a
    Nothing -> do
      putStr msg
      insist msg io

readHand :: String -> Maybe Hand
readHand "0" = Just Zero
readHand "1" = Just One
readHand "2" = Just Two
readHand "3" = Just Three
readHand "4" = Just Four
readHand "5" = Just Five
readHand _   = Nothing

handValue :: Hand -> Int
handValue Zero  = 0
handValue One   = 1
handValue Two   = 2
handValue Three = 3
handValue Four  = 4
handValue Five  = 5

readGuess :: String -> Maybe Guess
readGuess = mfilter (>=0) . readMaybe

readMode :: Char -> Maybe Mode
readMode 's' = Just Singleplayer
readMode 'm' = Just Multiplayer
readMode _   = Nothing

getMode :: IO Mode
getMode = do
  putStrLn "Main menu: "
  putStrLn "s: singleplayer"
  putStrLn "m: multiplayer"
  let mode = fmap readMode getChar
  insist "Incorrect mode (choose one of: s, m): " mode

getPlayer :: IO Player
getPlayer = do
  putStrLn "Player name: "
  let ioName = fmap (Just . Human) getLine
  insist "Name: " ioName

getHand :: IO Hand
getHand = do
  putStr "Hand: "
  let ioHand = fmap readHand getLine
  insist "Hand (number 0-5): " ioHand

getGuess :: IO Guess
getGuess = do
  putStr "Guess: "
  let ioGuess = fmap readGuess getLine
  insist "Guess (greater than 0): " ioGuess

getPlayerMove :: Player -> IO Move
getPlayerMove player = do
  printInterstitial
  putStrLn ("Make your move " ++ show player ++ "!")
  hand  <- getHand
  guess <- getGuess
  return (hand, guess)

getCPUMove :: [PlayerState] -> IO Move
getCPUMove states = do
  putStrLn "Computer prepares his move!"
  hand <- getRandomHand
  case getPredictedGuess states of
    Just guess ->
      return (hand, guess + handValue hand)
    Nothing -> do
      guess <- getRandomGuess states
      return (hand, guess)

getRandomHand :: IO Hand
getRandomHand = do
  random <- getStdRandom (randomR (0, 5 :: Int))
  case (readHand . show) random of
    Just hand -> return hand
    Nothing   -> do
      putStr "I made a mistake: "
      print random
      return One

getRandomGuess :: [PlayerState] -> IO Guess
getRandomGuess states = getStdRandom (randomR (0, (5 * (length states + 1)) :: Int))

getPredictedGuess :: [PlayerState] -> Maybe Guess
getPredictedGuess states =
  sumHandValues (traverse (predict . handValues) states)
    where handValues :: PlayerState -> [Int]
          handValues (PlayerState _ hs) = fmap handValue hs
          predict :: [Int] -> Maybe Int
          predict (m1:m2:m3:m4:m5:ms) =
            if m1 == m4 && m2 == m5 then Just m3
            else predict (m1:m2:m4:m5:ms)
          predict _ = Nothing
          sumHandValues :: Maybe [Int] -> Maybe Guess
          sumHandValues = fmap sum

getMove :: GameState -> Player -> IO Move
getMove gs p =
  if p == CPU then
    getCPUMove (M.elems (M.filterWithKey (\k _ -> k /= CPU) gs))
  else
    getPlayerMove p

getRetry :: IO Bool
getRetry = do
  let msg = "Continue? [yn]"
  putStrLn msg
  insist msg readAgreement
    where readAgreement = do
            c <- getChar
            case c of
              'y' -> return (Just True)
              'n' -> return (Just False)
              _   -> return Nothing

countHands :: Turn -> Int
countHands = sum . M.map (handValue . fst)

findWinners :: Turn -> Guess -> [Player]
findWinners turn count =
  M.keys (M.filter (\m -> snd m == count) turn)

updateState :: GameState -> Turn -> [Player] -> GameState
updateState old turn winners =
  M.mapWithKey updatePlayer old
    where updatePlayer player st =
            let score   = getScore st + if player `elem` winners then 1 else 0
                move    = fmap fst (M.lookup player turn)
                history = maybe (getHistory st) (:getHistory st) move
            in
                PlayerState score history


printScore :: Int -> [Player] -> GameState -> IO ()
printScore count winners gameState =
  let winnerDesc       = intercalate ", " (fmap show winners)
      scoreDesc (p, s) = show p ++ " -> " ++ show (getScore s)
      stateDesc        =
        intercalate ", " (map scoreDesc (M.toList gameState))
      turnDesc         =
        if null winners then "Nobody scored this turn!"
        else "Scored this turn: " ++ winnerDesc
  in do
    putStrLn ""
    putStrLn "============================================"
    putStrLn ("All hands show: " ++ show count)
    putStrLn turnDesc
    putStrLn ("SCORE: " ++ stateDesc)
    putStrLn "=============================================="
    putStrLn ""

printInterstitial :: IO ()
printInterstitial = do
  maybeWindow <- size
  let h = maybe (80 :: Int) height maybeWindow
  replicateM_ h (putStrLn ".")
  return ()

playGame :: StateT GameState IO ()
playGame = do
  gameState <- get
  turn <- lift (sequence (M.mapWithKey (\p _ -> getMove gameState p) gameState))
  let count = countHands turn
      winners = findWinners turn count
      newState = updateState gameState turn winners
  put newState
  liftIO (printScore count winners newState)
  retry <- lift getRetry
  when retry playGame

theWinner :: GameState -> (Player, Score)
theWinner gameState =
  let
    scoring (_, s1) (_, s2) = compare (getScore s1) (getScore s2)
  in
    fmap getScore (maximumBy scoring (M.toList gameState))

printWinner :: GameState -> IO ()
printWinner gameState = do
  let (winner, score) = theWinner gameState
  putStrLn $ concat
    [ "Aaaaaand the WINNER is... "
    , show winner
    , " with a score of "
    , show score
    ]

initialState :: [Player] -> GameState
initialState ps =
  M.fromList (fmap (\p -> (p, emptyState)) ps)
    where emptyState = PlayerState 0 []

main :: IO ()
main = do
  mode <- getMode
  putStrLn ""

  p1 <- getPlayer
  p2 <- if mode == Singleplayer then return CPU else getPlayer

  (_, result) <- runStateT playGame (initialState [p1, p2])

  putStrLn ""
  printWinner result
