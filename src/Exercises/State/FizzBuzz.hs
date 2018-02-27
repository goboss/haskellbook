module Exercises.State.FizzBuzz where

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo start end
  | start == end = []
  | otherwise    = fizzBuzz start : fizzbuzzFromTo (start + 1) end

main :: IO ()
main =
  mapM_ putStrLn $ fizzbuzzFromTo 1 100
