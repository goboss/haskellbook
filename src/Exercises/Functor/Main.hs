{-# LANGUAGE FlexibleInstances #-}

module Exercises.Functor.Main where

-- Write functor instances for the following

-- Exercise 1
data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- Exercise 2
data K a b = K a

instance Functor (K a) where
  fmap _ (K a) = K a

-- Exercise 3
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
newtype K' a b = K' a

instance Functor (Flip K' a) where
  fmap f (Flip (K' b)) = Flip (K' (f b))

-- Exercise 4
data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- Exercise 5
data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut x) = LiftItOut (fmap f x)

-- Exercise 6
data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa x y) = DaWrappa (fmap f x) (fmap f y)

-- Exercise 7
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething x y) = IgnoringSomething x (fmap f y)

-- Exercise 8
data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g b c) where
  fmap f (Notorious x y z) = Notorious x y (fmap f z)

-- Exercise 9
data List a = Nil | Cons a (List a)

instance Functor List where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  fmap _ Nil         = Nil

-- Exercise 10
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat            = NoGoat
  fmap f (OneGoat x)       = OneGoat (f x)
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

-- Exercise 11
data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read x)    = Read (fmap f x)
