module Exercises.Transformers.Morra where

import qualified Data.Map as M
import           Data.List (intercalate)
import           Data.Foldable (maximumBy)
import           Control.Applicative (liftA2)
import           Control.Monad (mfilter, when)
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class
import           System.Random (getStdRandom, randomR)
import           Text.Read (readMaybe)

type Player = String
type Guess = Int
type Score = Int
data Hand = Zero | One | Two | Three | Four | Five

type Move = (Hand, Guess)

type Turn = M.Map Player Move

type GameState = M.Map Player Score

cpuPlayer :: Player
cpuPlayer = "#Computer"

readHand :: String -> Maybe Hand
readHand "0" = Just Zero
readHand "1" = Just One
readHand "2" = Just Two
readHand "3" = Just Three
readHand "4" = Just Four
readHand "5" = Just Five
readHand _   = Nothing

handValue :: Hand -> Int
handValue Zero = 0
handValue One = 1
handValue Two = 2
handValue Three = 3
handValue Four = 4
handValue Five = 5

validGuess :: Int -> Bool
validGuess = (>=0)

readGuess :: String -> Maybe Guess
readGuess = mfilter validGuess . readMaybe

insist :: String -> IO (Maybe a) -> IO a
insist msg io =
  io >>= \ma ->
    case ma of
      Just a -> return a
      Nothing -> do
        putStr msg
        insist msg io

getPlayer :: IO Player
getPlayer = do
  putStrLn "Player name: "
  let ioName = fmap Just getLine
  insist "Name (different from \"CPU\"): " ioName

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
  putStrLn ("Make your move " ++ player ++ "!")
  hand  <- getHand
  guess <- getGuess
  return (hand, guess)

getCPUMove :: IO Move
getCPUMove = do
  putStrLn "Computer prepares his move!\n"
  let roll = getStdRandom (randomR (0, 5 :: Int))
  move <- liftA2 (,) (fmap (readHand . show) roll) (fmap (readGuess . show) roll)
  case move of
    (Just hand, Just guess) -> return (hand, guess)
    _                       -> do
      putStrLn "Computer had made a mistake!\n"
      return (One, 1)

getMove :: Player -> IO Move
getMove p = if p == cpuPlayer then getCPUMove else getPlayerMove p

countHands :: Turn -> Int
countHands = sum . M.map (handValue . fst)

findWinners :: Turn -> Guess -> [Player]
findWinners turn count =
  M.keys (M.filter (\m -> snd m == count) turn)

updateState :: GameState -> [Player] -> GameState
updateState old winners =
  M.mapWithKey (\p s -> if (p `elem` winners) then s + 1 else s) old

printScore :: Int -> [Player] -> GameState -> IO ()
printScore count winners gameState =
  let winnerDesc       = intercalate ", " winners
      scoreDesc (p, s) = show p ++ " -> " ++ show s
      stateDesc        =
        intercalate ", " (map scoreDesc (M.toList gameState))
  in
    putStrLn "\n============================================" >>
    putStrLn ("All hands show: " ++ show count) >>
    putStrLn (
      if null winners then "Nobody scored this turn!"
      else "Scored this turn: " ++ winnerDesc
    ) >>
    putStrLn ("SCORE: " ++ stateDesc) >>
    putStrLn "==============================================\n"

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

playGame :: StateT GameState IO ()
playGame = do
  gameState <- get
  turn <- lift (sequence  (M.mapWithKey (\p _ -> getMove p) gameState))
  let count = countHands turn
      winners = findWinners turn count
      newState = updateState gameState winners
  put newState
  liftIO (printScore count winners newState)
  retry <- lift getRetry
  when retry playGame

printWinner :: GameState -> IO ()
printWinner gameState = do
  let (winner, score) =  maximumBy (\(_,s1) (_,s2) -> compare s1 s2) (M.toList gameState)
  putStrLn (
    "Aaaaaand the WINNER is... " ++
    winner ++
    " with a score of " ++
    show score)

main :: IO ()
main = do
  player <- getPlayer
  let initialState = M.fromList [(player, 0), (cpuPlayer, 0)]
  (_, result) <- runStateT playGame initialState
  printWinner result
