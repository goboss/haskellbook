module Exercises.Testing.Generators where

import Test.QuickCheck

data Fool = Fulse | Frue deriving (Eq, Show)

-- Exercise 1
-- Equal probabilities for each.
foolGen :: Gen Fool
foolGen = elements [Fulse, Frue]

instance Arbitrary Fool where
  arbitrary = foolGen

-- Exercise 2
-- 2/3s chance of Fulse, 1/3 chance of Frue.
lessFrueGen :: Gen Fool
lessFrueGen = frequency [(2, return Fulse), (1, return Frue)]

