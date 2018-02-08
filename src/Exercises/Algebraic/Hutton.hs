module Exercises.Algebraic.Hutton where

data Expr = Lit Integer | Add Expr Expr

-- Exercise 1
-- Your first task is to write the “eval” function which reduces an
-- expression to a final sum.
fold :: (Integer -> b) -> (b -> b -> b) -> Expr -> b
fold f _ (Lit i)   = f i
fold f g (Add a b) = g (fold f g a) (fold f g b)

eval :: Expr -> Integer
eval = fold id (+)

-- Exercise 2
-- Write a printer for the expressions.
printExpr :: Expr -> String
printExpr = fold show (\a b -> a ++ " + " ++ b)
