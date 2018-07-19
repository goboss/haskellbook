module Exercises.Monoid.Laws where

import qualified Data.Semigroup as S

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

semigroupAssoc :: (Eq s, S.Semigroup s) => s -> s -> s -> Bool
semigroupAssoc a b c = (a S.<> (b S.<> c)) == ((a S.<> b) S.<> c)
