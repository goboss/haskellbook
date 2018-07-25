module Exercises.Libraries.Queue where

import Criterion.Main
import Data.Sequence (Seq(..), (<|), (><))
import qualified Data.Sequence as Seq

data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
        } deriving (Eq, Show)

empty :: Queue a
empty = Queue [] []

isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _             = False

fromList :: [a] -> Queue a
fromList = Queue []

-- adds an item
push :: a -> Queue a -> Queue a
push x (Queue en de) = Queue (x : en) de

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] [])     = Nothing
pop (Queue en [])     = pop (Queue [] (reverse en))
pop (Queue en (x:xs)) = Just (x, Queue en xs)

qq :: Queue a -> Queue a -> Queue a
qq (Queue le ld) (Queue re rd) = Queue (le ++ re) (ld ++ rd)

someData :: [Int]
someData = [1..10000]

someQueue :: Queue Int
someQueue = fromList someData

someSeq :: Seq Int
someSeq = Seq.fromList someData

alternatingQueue :: Int -> a -> Queue a -> Maybe (a, Queue a)
alternatingQueue n x q
  | n <= 0    = pop q
  | otherwise = alternatingQueue (n - 1) x next
      where next = 
              case pop (push x q) of
                Just (_, nq) -> nq
                Nothing      -> empty

alternatingList :: Int -> a -> [a] -> a
alternatingList n x l
  | n <= 0    = last l
  | otherwise = alternatingList (n - 1) x (init (x : l)) 

alternatingSeq :: Int -> a -> Seq a -> Maybe a
alternatingSeq n x s
  | n <= 0    = lst
  | otherwise = alternatingSeq (n - 1) x next
      where lst =
              case s of 
                (_ :|> l) -> Just l
                _         -> Nothing
            next =
              case s of 
                (h :|> _)    -> x <| h
                _            -> Seq.empty

drainQueue :: Queue a -> Maybe a
drainQueue q =
  case pop q of
    Nothing     -> Nothing
    Just (x, next) -> if isEmpty q then Just x else drainQueue next

drainList :: [a] -> Maybe a
drainList [] = Nothing
drainList xs =
  head xs `seq` drainList (tail xs)

drainSeq :: Seq a -> Maybe a
drainSeq Empty         = Nothing
drainSeq s =
  Seq.take 1 s `seq` drainSeq (Seq.drop 1 s)

main :: IO ()
main = defaultMain
  [ bench "queue push" $
    whnf (push 1) someQueue
  , bench "list push" $
    whnf (1:) someData
  , bench "seq push" $
    whnf (1<|) someSeq
  , bench "queue alternating push and pop" $
    whnf (alternatingQueue 100 1) someQueue
  , bench "list alternating push and pop" $
    whnf (alternatingList 100 1) someData
  , bench "seq alternating push and pop" $
    whnf (alternatingSeq 100 1) someSeq
  , bench "queue drain" $
    whnf drainQueue someQueue
  , bench "list drain" $
    whnf drainList someData
  , bench "seq drain" $
    whnf drainSeq someSeq
  , bench "queue concat" $
    whnf (qq someQueue) someQueue
  , bench "list concat" $
    whnf (someData++) someData
  , bench "seq concat" $
    whnf (someSeq ><) someSeq
  ]
