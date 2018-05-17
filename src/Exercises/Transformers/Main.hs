module Exercises.Transformers.Main where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity

-- Exercise 1,2
-- rDec is a function that should get its argument in the context of
-- Reader and return a value decremented by one.
-- Make it pointfree.
rDec :: Num a => Reader a a
rDec = reader $ flip (-) 1

-- Exercise 3, 4
-- rShow is show, but in Reader.
-- Make it pointfree.
rShow :: Show a => ReaderT a Identity String
rShow = reader show

-- Exercise 5
-- rPrintAndInc will first print the input with a greeting, then return
-- the input incremented by one.
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \a -> do
  putStr "Hi: "
  print a
  return (a + 1)

-- Exercise 6
-- sPrintIncAccum first prints the input with a greeting, then puts
-- the incremented input as the new state, and returns the original
-- input as a String.
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  putStr "Hi: "
  print s
  return (show s, s + 1)
