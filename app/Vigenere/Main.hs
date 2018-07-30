module Main where

import System.Environment (getArgs)
import System.IO
import System.Exit
import Exercises.Lists.Cipher

data Mode = Encrypt Keyword | Decrypt Keyword

getMode :: IO (Maybe Mode)
getMode =
  readMode <$> getArgs
    where readMode ["-d", key] = Just (Decrypt key)
          readMode ["-e", key] = Just (Encrypt key)
          readMode _           = Nothing

printUsage :: IO ()
printUsage = putStrLn $ concat
  [ "Usage: vigenere (-d | -e) key\n"
  , "Where -d means decrypt and -e encrypt"
  ]

interactImpatiently :: Int -> (String -> String) -> IO ()
interactImpatiently t f = do
  input <- hWaitForInput stdin t
  if input then do
    eof <- isEOF
    if eof then
      exitSuccess
    else do
      c <- getChar
      putStr (f [c])
      interactImpatiently t f
  else do
    hPutStrLn stderr "This takes too long. Let's bail before the cops show up!"
    exitFailure

patience :: Int
patience = 5000 -- 5 seconds

decrypt :: Keyword -> IO ()
decrypt key = interactImpatiently patience (vigenere key)

encrypt :: Keyword -> IO ()
encrypt key = interactImpatiently patience (unVigenere key)

run :: Mode -> IO ()
run (Decrypt key) = decrypt key
run (Encrypt key) = encrypt key

main :: IO ()
main = do
  mode <- getMode
  case mode of
    Nothing   -> printUsage
    Just m    -> run m
