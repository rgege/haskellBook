module Main where

import           Data.Monoid
import           Test.Hspec
import           Test.QuickCheck

import           Exercises       (Identity, Trivial, Two)


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

main :: IO ()
main =
  hspec $ do
    describe "Trivial Semigroup" $ do
      it "Associative" $ property (semigroupAssoc :: TrivialAssoc)
    describe "Identity Semigroup" $ do
      it "Associative" $ property (semigroupAssoc :: IdentityAssoc String)
    describe "Two Semigroup" $ do
      it "Associative" $ property (semigroupAssoc :: TwoAssoc String (Sum Int))

