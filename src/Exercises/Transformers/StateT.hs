module Exercises.Transformers.StateT where

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

-- Exercise 1
instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT g) = StateT $
    \s -> let
            mapFst (x, y) = (f x, y)
          in
            fmap mapFst (g s)


-- Exercise 2
instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  (StateT mf) <*> (StateT ma) = StateT $
    \s0 -> do
      (f, s1) <- mf s0
      (a, s2) <- ma s1
      return (f a, s2)

-- Exercise 3
instance (Monad m) => Monad (StateT s m) where
  return = pure
  (StateT sma) >>= f = StateT $
    \s0 -> do
      (a, s1) <- sma s0
      runStateT (f a) s1
