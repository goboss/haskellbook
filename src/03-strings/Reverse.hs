module Reverse where

rvrs :: String -> String
rvrs s = fst ++ snd ++ trd ++ "."
  where fst = drop 9 s
        snd = take 4 (drop 5 s)
        trd = take 5 s

main :: IO ()
main = print $ rvrs "Curry is awesome"

