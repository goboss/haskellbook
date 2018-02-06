module Nat where

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0  = Nothing
  | n == 0 = Just Zero
  | n > 0  = case integerToNat (n-1) of
              Nothing -> Nothing
              Just x -> Just (Succ(x))

