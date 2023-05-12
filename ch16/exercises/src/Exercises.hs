module Exercises where

import           Test.QuickCheck

--------------------------------------------------------------------------------
-- Functor Intermission 1
--------------------------------------------------------------------------------

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair l r)  = Pair (f l) (f r)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    l <- arbitrary
    r <- arbitrary
    return (Pair l r)

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)


