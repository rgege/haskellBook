import           Control.Monad (join)
-----------------------------------------------------------------
--Write bind in terms of fmap and join
-----------------------------------------------------------------
bind :: Monad m => (a -> m b) -> m a -> m b
bind g m = join $ fmap g m
-----------------------------------------------------------------
--Implement the Either Monad
-----------------------------------------------------------------
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a)  = First a
  fmap g (Second b) = Second $ g b

instance Applicative (Sum a) where
  pure x = Second x
  (<*>) (Second g) (Second a) = Second $ g a
  (<*>) _ (First a)           = First a
  (<*>) (First a) _           = First a

instance Monad (Sum a) where
  return = pure
  (>>=) (First a) _  = First a
  (>>=) (Second b) f = f b
-----------------------------------------------------------------
--Write the following functions using the methods provided by
--Monad and Functor
-----------------------------------------------------------------
--1.
j :: Monad m => m (m a) -> m a
j m = m >>= id
--2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m = m >>= return . f
--3.
l2 :: Monad m => (a, b, c) -> m a -> m b -> m c
l2 = undefined
--4.



