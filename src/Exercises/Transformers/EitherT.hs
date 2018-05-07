module Exercises.Transformers.EitherT where

import Data.Either (either)

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

-- Exercise 1
-- Write the Functor instance for EitherT.

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

-- Exercise 2
-- Write the Applicative instance for EitherT.

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  (EitherT emf) <*> (EitherT ema) = EitherT $ (fmap (<*>) emf) <*> ema

-- Exercise 3
-- Write the Monad instance for EitherT.

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT ema) >>= f = EitherT $ do
    v <- ema
    case v of
      (Left e)  -> return (Left e)
      (Right a) -> runEitherT $ f a

-- Exercise 4
-- Write the swapEitherT helper function for EitherT.

swapEither :: Either e a -> Either a e
swapEither (Left x)  = Right x
swapEither (Right x) = Left x

swapEitherT :: (Functor m)
            => EitherT e m a
            -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ fmap swapEither ema

-- Exercise 5
-- Write the transformer variant of the either catamorphism.
eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT fa fb (EitherT amb) = amb >>= either fa fb  
