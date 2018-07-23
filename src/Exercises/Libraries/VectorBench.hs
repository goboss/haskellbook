module Exercises.Libraries.VectorBench where

import Criterion.Main
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

theSameData :: [Int]
theSameData = take 10000 [1..]

boxed :: V.Vector Int
boxed = V.fromList theSameData

unboxed :: U.Vector Int
unboxed = U.fromList theSameData

testBoxed :: IO ()
testBoxed = defaultMain
  [ bench "fromList boxed" $
    nf V.fromList theSameData
  , bench "head boxed" $
    nf V.head boxed
  , bench "last boxed" $
    nf V.last boxed
  ]

testUnboxed :: IO ()
testUnboxed = defaultMain
  [ bench "fromList unboxed" $
    nf U.fromList theSameData
  , bench "head unboxed" $
    nf U.head unboxed
  , bench "last unboxed" $
    nf U.last unboxed
  ]
