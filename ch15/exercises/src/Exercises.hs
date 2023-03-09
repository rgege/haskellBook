module Exercises
  ( Trivial
  , Identity
  , Two
  , Three
  , Four
  , BoolConj
  , BoolDisj
  , Or
  , Combine
  , unCombine
  , Comp
  , unComp
  , Validation
  , Mem
  , runMem
  ) where

import           Test.QuickCheck hiding (Failure, Success)

--------------------------------------------------------------------------------
-- Semigroup exercises
--------------------------------------------------------------------------------

-- 1.

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) _ _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

-- 2.

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity a) (Identity a') = Identity (a <> a')

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

-- 3.

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two a b) (Two a' b') = Two (a <> a') (b <> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

-- 4.

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c)
         => Semigroup (Three a b c) where
          (<>) (Three a b c) (Three a' b' c')
            = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c)
         => Arbitrary (Three a b c) where
          arbitrary = Three <$> arbitrary
                            <*> arbitrary
                            <*> arbitrary

-- 5.

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
         => Semigroup (Four a b c d) where
          (<>) (Four a b c d) (Four a' b' c' d')
            = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
         => Arbitrary (Four a b c d) where
          arbitrary = Four <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary

-- 6.

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup (BoolConj) where
  (<>) (BoolConj True) (BoolConj True) = BoolConj True
  (<>) _               _               = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

-- 7.

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj False) (BoolDisj False) = BoolDisj False
  (<>) _                _                = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

-- 8.

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (<>) (Snd b) (Snd _) = Snd b
  (<>) _       (Snd b) = Snd b
  (<>) (Snd b) _       = Snd b
  (<>) _       (Fst a) = Fst a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = oneof [ Fst <$> arbitrary
                    , Snd <$> arbitrary ]

-- 9. sum of functions

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
  show (Combine _) = "Combine"


instance (Semigroup b) => Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine (f <> g)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

-- 10. composition of functions

newtype Comp a = Comp { unComp :: (a -> a) }

instance Show (Comp a) where
  show (Comp _) = "Comp"

instance (Semigroup a) => Semigroup (Comp a) where
  (<>) (Comp f) (Comp g) = Comp (f . g)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = Comp <$> arbitrary

-- 11.

data Validation a b =
  Failure a | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) (Failure a) (Failure a') = Failure (a <> a')
  (<>) _           (Success b)  = Success b
  (<>) (Success b)  _           = Success b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = oneof [ Failure <$> arbitrary
                    , Success <$> arbitrary ]
--------------------------------------------------------------------------------
-- Monoid exercises
--------------------------------------------------------------------------------

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance Monoid BoolConj where
  mempty  = BoolConj True
  mappend = (<>)

instance Monoid BoolDisj where
  mempty  = BoolDisj False
  mappend = (<>)

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine (const mempty)
  mappend = (<>)

instance Monoid a => Monoid (Comp a) where
  mempty = Comp id
  mappend = (<>)

-- 8.

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Show a => Show (Mem s a) where
  show (Mem _) = "Mem"

instance Semigroup a => Semigroup (Mem s a) where
  (<>) (Mem f) (Mem g) =
       Mem $ \s ->
        let (a', s') = g s
            (a'', s'') = f s'
        in  (a'' <> a', s'')

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend = (<>)

instance (CoArbitrary s, Arbitrary a, Arbitrary s) => Arbitrary (Mem s a) where
  arbitrary = Mem <$> arbitrary
