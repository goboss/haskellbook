module Exercises.Nonstrict.BottomIt where

-- Exercise
-- Using only bang patterns or seq, make the code bottom out when executed.

x = undefined
y = "blah"

main :: IO ()
main = do
  print (x `seq` snd (x, y))