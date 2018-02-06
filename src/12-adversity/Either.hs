module Either where

-- Exercise 1
-- Try to eventually arrive at a solution that uses foldr, even if
-- earlier versions don’t use foldr.
lefts' :: [Either a b] -> [a]
lefts' =
  let
    step (Right _) acc = acc
    step (Left a) acc  = a : acc
  in
    foldr step []

-- Exercise 2
-- Same as the last one. Use foldr eventually.
rights' :: [Either a b] -> [b]
rights' =
  let
    step (Left _) acc = acc
    step (Right b) acc  = b : acc
  in
    foldr step []

-- Exercise 3
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' =
  let
    step (Left a) (as, bs)  = (a:as, bs)
    step (Right b) (as, bs) = (as, b:bs)
  in
    foldr step ([],[])

-- Exercise 4
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just (f b)

-- Exercise 5
-- This is a general catamorphism for Either values.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)     =  f a
either' _ g (Right b)    =  g b

-- Exercise 6
-- Same as before, but use the either' function you just wrote.
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (Just . f)

