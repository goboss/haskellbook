module Main where

import System.Environment (getArgs)
import System.IO
import Exercises.Lists.Cipher

data Mode = Encrypt Keyword | Decrypt Keyword

getMode :: IO (Maybe Mode)
getMode = do
  args <- getArgs
  return (readMode args)
    where readMode ["-d", key] = Just (Decrypt key)
          readMode ["-e", key] = Just (Encrypt key)
          readMode _           = Nothing 

printUsage :: IO ()
printUsage = putStrLn $ concat
  [ "Usage: vigenere (-d | -e) key\n"
  , "Where -d means decrypt and -e encrypt"
  ]

decrypt :: Keyword -> IO ()
decrypt key = interact go
  where go = vigenere key

encrypt :: Keyword -> IO ()
encrypt key = interact go
  where go = unVigenere key

run :: Mode -> IO ()
run (Decrypt key) = decrypt key
run (Encrypt key) = encrypt key

main :: IO ()
main = do
  mode <- getMode
  case mode of
    Nothing   -> printUsage
    Just m    -> run m
