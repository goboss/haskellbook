module Maybe where

-- Exercise 1
-- Simple boolean checks for Maybe values.
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _      = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- Exercise 2
-- The following is the Maybe catamorphism. You can turn a Maybe
-- value into anything else with this.
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee z _ Nothing  = z
mayybee _ f (Just a) = f a

-- Exercise 3
-- In case you just want to provide a fallback value.
fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing  = x
fromMaybe _ (Just a) = a

-- Exercise 4
-- Converting between List and Maybe.
listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

-- Exercise 5
-- For when we want to drop the Nothing values from our list.
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes ((Just a):xs) = a : catMaybes xs

-- Exercise 6
-- You’ll see this called “sequence” later.
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe =
  let
    step _ Nothing           = Nothing
    step Nothing _           = Nothing
    step (Just a) (Just acc) = Just (a:acc)
  in
    foldr step (Just [])

