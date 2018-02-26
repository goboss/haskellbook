module Exercises.Reader.ReaderPractice where

import Prelude hiding (lookup, uncurry)

x :: [Integer]
x = [1, 2, 3]
y :: [Integer]
y = [4, 5, 6]
z :: [Integer]
z = [7, 8, 9]

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup _ [] = Nothing
lookup a ((a', b) : rest)
  | a == a'   = Just b
  | otherwise = lookup a rest

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 (zip x y)

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 (zip y z)

-- it's also nice to have one that will return Nothing, like this one
-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- now zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

-- Have x1 make a tuple of xs and ys, and x2 make a tuple of of ys and zs.
-- Also, write x3 which takes one input and makes a tuple of the results
--of two applications of z' from above.
x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)

-- Next, we’re going to make some helper functions. Let’s use uncurry
-- to allow us to add the two values that are inside a tuple:
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry abc (a, b) = abc a b

summed :: Num c => (c, c) -> c
summed = uncurry (+)

-- And now we’ll make a function similar to some we’ve seen before
-- that lifts a boolean function over two partially applied functions:
bolt :: Integer -> Bool
-- use &&, >3, <8
bolt = (&&) <$> (>3) <*> (<8)

-- Finally, we’ll be using fromMaybe in the main exercise, so let’s look
-- at that:
fromMaybe :: a -> Maybe a -> a
fromMaybe d Nothing  = d
fromMaybe _ (Just a) = a

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just (1 :: Integer)]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] (7 :: Integer)
  -- Exercise
  print $ foldr (&&) True (sequA (42 :: Integer))
  print $ sequA $ fromMaybe 0 s'
  print $ bolt  $ fromMaybe 0 ys
