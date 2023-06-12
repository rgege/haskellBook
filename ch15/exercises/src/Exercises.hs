module Exercises ( First'
                 , Trivial
                 , Identity
                 , Two
                 , BoolConj
                 , BoolDisj
                 , Or
                 , Combine
                 , unCombine
                 , Compose
                 , unCompose
                 , Validation
                 , AccumulateRight
                 , AccumulateBoth
                 ) where

import           Test.QuickCheck hiding (Failure, Success)

------------------------------------------------------------------
--Intermission: Exercises
------------------------------------------------------------------
data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Semigroup (First' a) where
  First' Nada     <> First' Nada     = First' Nada
  First' (Only a) <> _               = First' (Only a)
  _               <> First' (Only a) = First' (Only a)

instance Monoid (First' a) where
  mempty = First' Nada

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary =
    frequency [ (1, return Nada)
              , (2, Only <$> arbitrary) ]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = First' <$> arbitrary

------------------------------------------------------------------
--Chapter Exercises
------------------------------------------------------------------
--1.
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) _ _ = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial
--2.
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity a) (Identity a') = Identity (a <> a')

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary
--3.
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two a b) (Two a' b') = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

--4.
--5.
--
--6.
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) (BoolConj True) (BoolConj True) = BoolConj True
  (<>) _ _                             = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = frequency [ (1, return (BoolConj True))
                        , (1, return (BoolConj False)) ]
--7.
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj False) (BoolDisj False) = BoolDisj False
  (<>) _ _                               = BoolDisj True

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = frequency [ (1, return (BoolDisj True))
                        , (1, return (BoolDisj False)) ]
--8.
data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  (<>) (Snd b) _ = Snd b
  (<>) _ (Snd b) = Snd b
  (<>) _ (Fst a) = Fst a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = frequency [ (1, (Fst <$> arbitrary))
                        , (1, (Snd <$> arbitrary)) ]
--9.
newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance Show b => Show (Combine a b) where
  show _  = "Combine"

instance (Eq b, Semigroup b) => Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine (f <> g)

instance (Eq b, Monoid b) => Monoid (Combine a b) where
  mempty = Combine mempty

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary
--10.
newtype Compose a =
  Compose { unCompose :: (a -> a) }

instance Show (Compose a) where
  show _ = "Compose"

instance (Eq a, Semigroup a) => Semigroup (Compose a) where
  (<>) (Compose f) (Compose g) = Compose (f . g)

instance (Eq a, Monoid a) => Monoid (Compose a) where
  mempty = Compose id

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Compose a) where
  arbitrary = Compose <$> arbitrary
--11.
data Validation a b =
    Failure a
  | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) (Failure a) (Failure a') = Failure (a <> a')
  (<>) (Success b) _            = Success b
  (<>) _           (Success b)  = Success b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = frequency [ (1, Success <$> arbitrary)
                        , (1, Failure <$> arbitrary) ]
--12.
newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  AccumulateRight (Success b) <> AccumulateRight (Success b') = AccumulateRight $ Success (b <> b')
  AccumulateRight (Failure a) <> _                            = AccumulateRight (Failure a)
  _                           <> AccumulateRight (Failure a)  = AccumulateRight (Failure a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = frequency [ (1, AccumulateRight <$> arbitrary)
                        , (1, AccumulateRight <$> arbitrary) ]
--13.
newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  AccumulateBoth (Failure a) <> AccumulateBoth  (Failure a') = AccumulateBoth  $ Failure (a <> a')
  AccumulateBoth (Success b) <> AccumulateBoth (Success b')  = AccumulateBoth  $ Success (b <> b')
  AccumulateBoth (Success b) <> _                            = AccumulateBoth  $ Success b
  _                          <> AccumulateBoth (Success b)   = AccumulateBoth  $ Success b

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = frequency [ (1, AccumulateBoth <$> arbitrary)
                        , (1, AccumulateBoth <$> arbitrary) ]

