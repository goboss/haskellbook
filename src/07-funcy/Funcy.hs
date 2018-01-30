module Funcy where

-- Chapter exercises

-- Exercise 1
tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = fst . divMod x $ 10
        d     = snd . divMod xLast $ 10

hunsD :: Integral a => a -> a
hunsD x = d
  where xLast = fst . divMod x $ 100
        d     = snd . divMod xLast $ 10

-- Exercise 2
foldBool :: a -> a -> Bool -> a
foldBool x y b =
  case b of
    True  -> x
    False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y b
  | b == True  = x
  | otherwise  = y

-- Exercise 3
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

-- Exercise 5, 6
roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

