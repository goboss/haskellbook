module Exercises.State.Moi where

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

-- Exericise 1
-- Implement the Functor instance for State.

instance Functor (Moi s) where
  fmap f (Moi g) = Moi $ \s -> let (a, s1) = g s
                               in (f a, s1)

-- Exercise 2
-- Write the Applicative instance for State.
instance Applicative (Moi s) where
  pure a = Moi (\s -> (a, s))

  (Moi f) <*> (Moi g) = Moi $ \s -> let (ab, s1)  = f s
                                        (a,  s2)  = g s1
                                    in  (ab a, s2)

-- Exercise 3
-- Write the Monad instance for State.
instance Monad (Moi s) where
  return = pure

  (Moi f) >>= g = Moi $ \s -> let (a, s1) = f s
                                  mb      = runMoi (g a)
                              in mb s1

-- Chapter Exercises
get :: Moi s s
get = Moi (\s -> (s, s))

put :: s -> Moi s ()
put s = Moi (\_ -> ((), s))

exec :: Moi s a -> s -> s
exec (Moi sa) s = snd $ sa s

eval :: Moi s a -> s -> a
eval (Moi sa) s = fst $ sa s

modify :: (s -> s) -> Moi s ()
modify f = Moi (\s -> ((), f s))
