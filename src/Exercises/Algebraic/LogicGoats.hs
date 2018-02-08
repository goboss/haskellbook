{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Exercises.Algebraic.LogicGoats where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- Exercise 1
-- Reusing the TooMany typeclass, write an instance of the typeclass
-- for the type (Int, String).
instance TooMany (Int, String) where
  tooMany (i, _) = tooMany i

-- Exercise 2
-- Make another TooMany instance for (Int, Int). Sum the values
-- together under the assumption this is a count of goats from two fields.
instance TooMany (Int, Int) where
  tooMany (i, j) = tooMany $ i + j

-- Exercise 3
-- Make another TooMany instance, this time for (Num a, TooMany a) => (a, a).
-- This can mean whatever you want, such as summing the two numbers together.
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany x || tooMany y
