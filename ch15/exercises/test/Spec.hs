module Main (main) where

import           Data.Monoid
import           Exercises       (AccumulateBoth, AccumulateRight, BoolConj,
                                  BoolDisj, Combine, Compose, First', Identity,
                                  Or, Trivial, Two, Validation, unCombine,
                                  unCompose)
import           Test.Hspec
import           Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

combSemigroupAssoc :: (Eq m, Semigroup m) => Combine a m -> Combine a m -> Combine a m -> a -> Bool
combSemigroupAssoc f g h a =
  unCombine (f <> (g <> h)) a == unCombine ((f <> g) <> h) a

combMonoidAssoc :: (Eq m, Monoid m) => Combine a m -> Combine a m -> Combine a m -> a -> Bool
combMonoidAssoc f g h a = unCombine (f <> (g <> h)) a == unCombine ((f <> g) <> h) a

combMonoidLeftIdentity :: (Eq m, Monoid m) => Combine a m -> a -> Bool
combMonoidLeftIdentity f a = unCombine (mempty <> f) a == unCombine (f) a

combMonoidRightIdentity :: (Eq m, Monoid m) => Combine a m -> a -> Bool
combMonoidRightIdentity f a = unCombine (f <> mempty) a == unCombine (f) a

compSemigroupAssoc :: (Eq m, Semigroup m) => Compose m -> Compose m -> Compose m -> m -> Bool
compSemigroupAssoc f g h a =
  unCompose (f <> (g <> h)) a == unCompose ((f <> g) <> h) a

compMonoidAssoc :: (Eq m, Monoid m) => Compose m -> Compose m -> Compose m -> m -> Bool
compMonoidAssoc f g h a = unCompose (f <> (g <> h)) a == unCompose ((f <> g) <> h) a

compMonoidLeftIdentity :: (Eq m, Monoid m) => Compose m -> m -> Bool
compMonoidLeftIdentity f a = unCompose (mempty <> f) a == unCompose (f) a

compMonoidRightIdentity :: (Eq m, Monoid m) => Compose m -> m -> Bool
compMonoidRightIdentity f a = unCompose (f <> mempty) a == unCompose (f) a

type FirstMappend = First' String -> First' String -> First' String -> Bool

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

type TwoAssoc = Two (Sum Int) String -> Two (Sum Int) String -> Two (Sum Int) String -> Bool

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

type OrAssoc = Or (Sum Int) (Sum Int) -> Or (Sum Int) (Sum Int) -> Or (Sum Int) (Sum Int) -> Bool

type CombineAssoc =  Combine (Sum Int) (Sum Int)
                  -> Combine (Sum Int) (Sum Int)
                  -> Combine (Sum Int) (Sum Int)
                  -> (Sum Int)
                  -> Bool

type ComposeAssoc =  Compose (Sum Int)
                  -> Compose (Sum Int)
                  -> Compose (Sum Int)
                  -> (Sum Int)
                  -> Bool

type ValidationAssoc =  Validation String String
                     -> Validation String String
                     -> Validation String String
                     -> Bool

type AccumulateRightAssoc =  AccumulateRight String String
                          -> AccumulateRight String String
                          -> AccumulateRight String String
                          -> Bool

type AccumulateBothAssoc = AccumulateBoth String String
                         -> AccumulateBoth String String
                         -> AccumulateBoth String String
                         -> Bool


main :: IO ()
main = hspec $ do
  describe "First' Monoid" $ do
    it "Associativity"  $ property (monoidAssoc :: FirstMappend)
    it "Left Identity"  $ property (monoidLeftIdentity :: First' String -> Bool)
    it "Right Identity" $ property (monoidRightIdentity :: First' String -> Bool)
  describe "First' Monoid & Semigroup" $ do
    it "Semigroup Associativity"  $ property (semigroupAssoc :: TrivialAssoc)
    it "Monoid Associativity"     $ property (monoidAssoc :: TrivialAssoc)
    it "Left Identity"            $ property (monoidLeftIdentity :: Trivial -> Bool)
    it "Right Identity"           $ property (monoidRightIdentity :: Trivial -> Bool)
  describe "Identity Monoid & Semigroup" $ do
    it "Semigroup Associativity"  $ property (semigroupAssoc :: IdentityAssoc)
    it "Monoid Associativity"     $ property (monoidAssoc :: IdentityAssoc)
    it "Left Identity"            $ property (monoidLeftIdentity :: Identity String -> Bool)
    it "Right Identity"           $ property (monoidRightIdentity :: Identity String -> Bool)
  describe "Two Monoid & Semigroup" $ do
    it "Semigroup Associativity"  $ property (semigroupAssoc :: TwoAssoc)
    it "Monoid Associativity"     $ property (monoidAssoc :: TwoAssoc)
    it "Left Identity"            $ property (monoidLeftIdentity :: Two String String -> Bool)
    it "Right Identity"           $ property (monoidRightIdentity :: Two String String -> Bool)
  describe "BoolConj Monoid & Semigroup" $ do
    it "Semigroup Associativity"  $ property (semigroupAssoc :: BoolConjAssoc)
    it "Monoid Associativity"     $ property (monoidAssoc :: TwoAssoc)
    it "Left Identity"            $ property (monoidLeftIdentity :: BoolConj -> Bool)
    it "Right Identity"           $ property (monoidRightIdentity :: BoolConj -> Bool)
  describe "BoolDisj Monoid & Semigroup" $ do
    it "Semigroup Associativity"  $ property (semigroupAssoc :: BoolDisjAssoc)
    it "Monoid Associativity"     $ property (monoidAssoc :: BoolDisjAssoc)
    it "Left Identity"            $ property (monoidLeftIdentity :: BoolDisj -> Bool)
    it "Right Identity"           $ property (monoidRightIdentity :: BoolDisj -> Bool)
  describe "Or Semigroup" $ do
    it "Semigroup Associativity"  $ property (semigroupAssoc :: OrAssoc)
  describe "Combine Semigroup & Monoid" $ do
    it "Semigroup Associativity"  $ property (combSemigroupAssoc :: CombineAssoc)
    it "Monoid Associativity"     $ property (combMonoidAssoc :: CombineAssoc)
    it "Left Identity"            $ property (combMonoidLeftIdentity :: Combine (Sum Int) (Sum Int)
                                                                        -> (Sum Int) -> Bool)
    it "Right Identity"           $ property (combMonoidRightIdentity :: Combine (Sum Int) (Sum Int)
                                                                         -> (Sum Int) -> Bool)
  describe "Compose Semigroup & Monoid" $ do
    it "Semigroup Associativity"  $ property (compSemigroupAssoc :: ComposeAssoc)
    it "Monoid Associativity"     $ property (compMonoidAssoc :: ComposeAssoc)
    it "Left Identity"            $ property (compMonoidLeftIdentity :: Compose (Sum Int)
                                                                        -> (Sum Int) -> Bool)
    it "Right Identity"           $ property (compMonoidRightIdentity :: Compose (Sum Int)
                                                                         -> (Sum Int) -> Bool)
  describe "Validation Semigroup" $ do
    it "Semigroup Associativity"  $ property (semigroupAssoc :: ValidationAssoc)
  describe "AccumulateRight Semigroup" $ do
    it "Semigroup Associativity"  $ property (semigroupAssoc :: AccumulateRightAssoc)
  describe "AccumulateBoth Semigroup" $ do
    it "Semigroup Associativity"  $ property (semigroupAssoc :: AccumulateBothAssoc)
