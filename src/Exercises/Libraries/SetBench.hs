module Exercises.Libraries.SetBench where

import Criterion.Main
import qualified Data.Map as M
import qualified Data.Set as S

-- Exercise
-- Make a benchmark to prove for yourself whether Map and Set have
-- similar performance. Try operations other than membership testing,
-- such as insertion or unions.

bumpIt :: (Int, Int) -> (Int, Int)
bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
  where stream = iterate bumpIt (0, 0)

m' :: M.Map Int Int
m' = M.map (*(-1)) m


s :: S.Set Int
s = S.fromList $ take 10000 stream
  where stream = iterate (+1) 0

s' :: S.Set Int
s' = S.map (*(-1)) s

membersMap :: Int -> Bool
membersMap i = M.member i m

membersSet :: Int -> Bool
membersSet i = S.member i s

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

main :: IO ()
main = defaultMain
  [ bench "member check map" $
    whnf membersMap 9999
  , bench "member check set" $
    whnf membersSet 9999
  , bench "insert check map" $
    whnf (M.insert 10000 10000) m
  , bench "insert check set" $
    whnf (S.insert 10000) s
  , bench "delete check map" $
    whnf (M.delete 9999) m
  , bench "delete check set" $
    whnf (S.delete 9999) s
  , bench "union check map" $
    whnf (M.union m) m'
  , bench "union check set" $
    whnf (S.union s) s'
  , bench "intersection check map" $
    whnf (M.intersection m) m'
  , bench "intersection check set" $
    whnf (S.intersection s) s'
  , bench "filter check map" $
    whnf (M.filter isEven) m
  , bench "filter check set" $
    whnf (S.filter isEven) s
  , bench "map check map" $
    whnf (M.map show) m
  , bench "map check set" $
    whnf (S.map show) s
  ]
