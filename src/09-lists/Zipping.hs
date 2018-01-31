module Zipping where

-- Write your own versions of zip and myZipWith

myZip :: [a] -> [b] -> [(a, b)]
myZip = myZipWith (,)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys
myZipWith _ _ _ = []

