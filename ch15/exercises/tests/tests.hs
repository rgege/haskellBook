module Main where

import           Data.Monoid
import           Test.Hspec
import           Test.QuickCheck

import           Exercises       (BoolConj, BoolDisj, Combine, Comp, Four,
                                  Identity, Mem, Or, Three, Trivial, Two,
                                  Validation, runMem, unCombine, unComp)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

combineAssoc :: (Eq b, Semigroup b) => Combine a b -> Combine a b -> Combine a b -> a -> Bool
combineAssoc f g h n = unCombine (f <> (g <> h)) n == unCombine ((f <> g) <> h) n

compAssoc :: (Eq a, Semigroup a) => Comp a -> Comp a -> Comp a -> a -> Bool
compAssoc f g h n = unComp (f <> (g <> h)) n == unComp ((f <> g) <> h) n

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool

type FourAssoc a b c d = Four a b c d -> Four a b c d -> Four a b c d -> Bool

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

type CombineAssoc a b = Combine a b -> Combine a b -> Combine a b -> a -> Bool

type CompAssoc a = Comp a -> Comp a -> Comp a -> a -> Bool

type ValidationAssoc a b = Validation a b -> Validation a b -> Validation a b -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

monoidLeftCombine :: (Eq b, Monoid b) => Combine a b -> a -> Bool
monoidLeftCombine f n = unCombine (mempty <> f) n == unCombine f n

monoidRightCombine :: (Eq b, Monoid b) => Combine a b -> a -> Bool
monoidRightCombine f n = unCombine (f <> mempty) n == unCombine f n

monoidLeftComp :: (Eq a, Monoid a) => Comp a -> a -> Bool
monoidLeftComp f n = unComp (mempty <> f) n == unComp f n

monoidRightComp :: (Eq a, Monoid a) => Comp a -> a -> Bool
monoidRightComp f n = unComp (f <> mempty) n == unComp f n

memMonoidAssoc ::
     (Eq s, Eq a, Monoid a) => Mem s a -> Mem s a -> Mem s a -> s -> Bool
memMonoidAssoc f g h a = runMem (f <> (g <> h)) a == runMem ((f <> g) <> h) a

memLeftIdentity :: (Eq s, Eq a, Monoid a) => Mem s a -> s -> Bool
memLeftIdentity f a = runMem (mempty <> f) a == runMem f a

memRightIdentity :: (Eq s, Eq a, Monoid a) => Mem s a -> s -> Bool
memRightIdentity f a = runMem (f <> mempty) a == runMem f a

type TrivialMonoid = Trivial -> Bool

type IdentityMonoid a = Identity a -> Bool

type BoolConjMonoid = BoolConj -> Bool

type BoolDisjMonoid = BoolDisj -> Bool

type CombineMonoid a b = Combine a b -> a -> Bool

type CompMonoid a = Comp a -> a -> Bool

type MemAssoc s a = Mem s a -> Mem s a -> Mem s a -> s -> Bool

type MemId s a = Mem s a -> s -> Bool

main :: IO ()
main =
  hspec $ do
    describe "Trivial Semigroup" $ do
      it "Associative" $ property (semigroupAssoc :: TrivialAssoc)
    describe "Identity Semigroup" $ do
      it "Associative" $ property (semigroupAssoc :: IdentityAssoc String)
    describe "Two Semigroup" $ do
      it "Associative" $ property (semigroupAssoc :: TwoAssoc String (Sum Int))
    describe "Three Semigroup" $ do
      it "Associative" $ property (semigroupAssoc
        :: ThreeAssoc String (Sum Int) (Product Int))
    describe "Four Semigroup" $ do
      it "Associative" $ property (semigroupAssoc
        :: FourAssoc String (Sum Int) (Product Int) [Bool])
    describe "BoolConj Semigroup" $ do
      it "Associative" $ property (semigroupAssoc :: BoolConjAssoc)
    describe "BoolDisj Semigroup" $ do
      it "Associative" $ property (semigroupAssoc :: BoolDisjAssoc)
    describe "Or Semigroup" $ do
      it "Associative" $ property (semigroupAssoc :: OrAssoc (Product Int) (Sum Int))
    describe "Combine Semigroup" $ do
      it "Associative" $ property (combineAssoc :: CombineAssoc (Product Int) (Sum Int))
    describe "Composition Semigroup" $ do
      it "Associative" $ property (compAssoc :: CompAssoc (Sum Int))
    describe "Validation Semigroup" $ do
      it "Associative" $ property (semigroupAssoc :: ValidationAssoc String Int)
    describe "Trivial Monoid" $ do
      it "Associative" $ property (monoidAssoc :: TrivialAssoc)
      it "Left Identity" $ property (monoidLeftIdentity :: TrivialMonoid)
      it "Right Identity" $ property (monoidRightIdentity :: TrivialMonoid)
    describe "Identity Monoid" $ do
      it "Associative" $ property (monoidAssoc :: IdentityAssoc String)
      it "Left Identity" $ property (monoidLeftIdentity :: IdentityMonoid String)
      it "Right Identity" $ property (monoidRightIdentity :: IdentityMonoid String)
    describe "BoolConj Monoid" $ do
      it "Associative" $ property (monoidAssoc :: BoolConjAssoc)
      it "Left Identity" $ property (monoidLeftIdentity :: BoolConjMonoid)
      it "Right Identity" $ property (monoidRightIdentity :: BoolConjMonoid)
    describe "BoolDisj Monoid" $ do
      it "Associative" $ property (monoidAssoc :: BoolDisjAssoc)
      it "Left Identity" $ property (monoidLeftIdentity :: BoolDisjMonoid)
      it "Right Identity" $ property (monoidRightIdentity :: BoolDisjMonoid)
    describe "Combine Monoid" $ do
      it "Associative" $ property (combineAssoc :: CombineAssoc (Sum Int) (Product Int))
      it "Left Identity" $ property (monoidLeftCombine :: CombineMonoid (Product Int) (Sum Int))
      it "Right Identity" $ property (monoidRightCombine :: CombineMonoid (Product Int) (Sum Int))
    describe "Composition Monoid" $ do
      it "Associative" $ property (compAssoc :: CompAssoc (Sum Int))
      it "Left Identity" $ property (monoidLeftComp :: CompMonoid (Sum Int))
      it "Right Identity" $ property (monoidRightComp :: CompMonoid (Sum Int))
    describe "Mem Monoid" $ do
      it "Associative" $ property (memMonoidAssoc :: MemAssoc Float (Product Int))
      it "Left identity" $ property (memLeftIdentity :: MemId Float (Product Int))
      it "Right identity" $ property (memRightIdentity :: MemId Float (Product Int))
