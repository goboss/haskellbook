module Exercises.Transformers.MonadIO where

import Control.Monad
import Control.Monad.IO.Class

-- Exercise 1
newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT (pure (pure x))
  MaybeT fab <*> MaybeT mma = MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m) => Monad (MaybeT m) where
  return = pure

  MaybeT ma >>= f = MaybeT $ do
    v <- ma
    case v of
      Nothing -> return Nothing
      Just y -> runMaybeT (f y)

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = MaybeT . liftM Just . liftIO

-- Exercise 2
newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure x = ReaderT $ pure (pure x)
  ReaderT frma <*> ReaderT rma = ReaderT $ (<*>) <$> frma <*> rma

instance Monad m => Monad (ReaderT r m) where
  return = pure

  ReaderT rma >>= f = ReaderT $ \r -> do
    a <- rma r
    runReaderT (f a) r

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO io = ReaderT $ const (liftIO io)

-- Exercise 3
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
  (StateT sma) >>= f = StateT $ \s0 -> do
    (a, s1) <- sma s0
    runStateT (f a) s1

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO io = StateT $ \s -> do
    a <- liftIO io
    return (a, s)
