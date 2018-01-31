module SquareCube where

mySqr :: [Integer]
mySqr = [x^2 | x <- [1..5]]

myCube :: [Integer]
myCube = [y^3 | y <- [1..5]]

-- Exercise 1: First write an expression that will make tuples of the outputs of
-- mySqr and myCube.
tuples :: [(Integer, Integer)]
tuples = [(x, y) | x <- mySqr, y <- myCube]

-- Exercise 2: Now alter that expression so that it only uses the x and y values
-- that are less than 50.
tuplesLT50 :: [(Integer, Integer)]
tuplesLT50 = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- Exercise 3: Apply another function to that list comprehension to determine
-- how many tuples inhabit your output list.
howMany :: Int
howMany = length tuplesLT50

