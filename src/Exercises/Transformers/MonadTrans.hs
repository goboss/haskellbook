module Exercises.Transformers.MonadTrans where

import Control.Monad (liftM)
import Control.Monad.Trans.Class

-- Exercise 1
newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  (EitherT emf) <*> (EitherT ema) = EitherT $ (fmap (<*>) emf) <*> ema

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT ema) >>= f = EitherT $ do
    v <- ema
    case v of
      (Left e)  -> return (Left e)
      (Right a) -> runEitherT $ f a

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

-- Exercise 2
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT g) = StateT $
    \s -> let
            mapFst (x, y) = (f x, y)
          in
            fmap mapFst (g s)

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  (StateT mf) <*> (StateT ma) = StateT $
    \s0 -> do
      (f, s1) <- mf s0
      (a, s2) <- ma s1
      return (f a, s2)

instance (Monad m) => Monad (StateT s m) where
  return = pure
  (StateT sma) >>= f = StateT $
    \s0 -> do
      (a, s1) <- sma s0
      runStateT (f a) s1

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> fmap (\a -> (a, s)) ma
