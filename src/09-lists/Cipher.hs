module Cipher where

import Data.Char

shift :: Int -> Char -> Char
shift n c
  | isAlpha c = chr $ mod (ord c - minChar c + n) (maxChar c - minChar c + 1) + minChar c
  | otherwise  = c
  where
    maxChar c = ord $ if (isUpper c) then 'Z' else 'z'
    minChar c = ord $ if (isUpper c) then 'A' else 'a'

-- Exercise
-- Implement Caesar cipher
caesar :: Int -> String -> String
caesar n = map (shift n)

unCaesar :: Int -> String -> String
unCaesar = caesar . negate

caesarIO :: Int -> IO String
caesarIO n = do
  putStrLn "Enter your message:"
  msg <- getLine
  return $ caesar n msg

-- Exercise
-- Implement Vigenere cipher
type Keyword = String

vigenere :: Keyword -> String -> String
vigenere key str = go lowerKey str ""
  where lowerKey = map toLower key
        go _ [] acc  = reverse acc
        go [] ss acc = go lowerKey ss acc
        go ke@(k:ks) (s:ss) acc
          | isAlpha s = go ks ss ((shift (ord k - ord 'a') s) : acc)
          | otherwise = go ke ss (s : acc)

unVigenere :: Keyword -> String -> String
unVigenere key = vigenere unkey
  where unkey = map ((\c -> shift (ord 'a' - ord c) 'a') . toLower) key

vigenereIO :: IO String
vigenereIO = do
  putStrLn "Enter the keyword:"
  keyword <- getLine
  putStrLn "Enter your message:"
  msg <- getLine
  return $ vigenere keyword msg

unVigenereIO :: IO String
unVigenereIO = do
  putStrLn "Enter the keyword:"
  keyword <- getLine
  putStrLn "Enter the cipher:"
  cipher <- getLine
  return $ unVigenere keyword cipher

