module Exercises where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers

data CountMe a =
  CountMe Integer a
  deriving (Eq, Show)

instance Functor CountMe where
  fmap g (CountMe i a) = CountMe i (g a)

instance Applicative CountMe where
  pure = CountMe 0
  (<*>) (CountMe n f) (CountMe n' a) = CountMe (n + n') (f a)

instance Monad CountMe where
  return = pure

  CountMe n a >>= f =
    let CountMe n' b = f a
    in CountMe (n + n') b

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

------------------------------------------------------------------
--Chapter Exercises
------------------------------------------------------------------
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) NopeDotJpg _ = NopeDotJpg
------------------------------------------------------------------
data PhbtEither b a =
    Left' a
  | Right' b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhbtEither b a) where
  arbitrary = frequency [(1, Left' <$> arbitrary),
                         (2, Right' <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (PhbtEither b a) where
  (=-=) = eq

instance Functor (PhbtEither b) where
  fmap _ (Right' b) = Right' b
  fmap g (Left' a)  = Left' $ g a

instance Applicative (PhbtEither b) where
  pure = Left'
  (<*>) (Right' b) _        = Right' b
  (<*>) _ (Right' b)        = Right' b
  (<*>) (Left' f) (Left' a) = Left' $ f a

instance Monad (PhbtEither b) where
  return = pure
  (>>=) (Left' a) f  = f a
  (>>=) (Right' b) _ = Right' b
------------------------------------------------------------------
data Identity a = Identity a deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary
