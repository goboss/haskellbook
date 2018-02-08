module Exercises.Adversity.BinaryTree where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- Exercise 1
-- Write unfold for BinaryTree.
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x =
  case f x of
    Nothing      -> Leaf
    Just (l,a,r) -> Node (unfold f l) a (unfold f r)

-- Exercise 2
-- Make a tree builder.
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\x -> if x < n then Just (x + 1, x, x + 1) else Nothing) 0
