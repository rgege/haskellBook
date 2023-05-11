module Main where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import           Exercises

main :: IO ()
main =
  hspec $ do
    describe "CountMe" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: CountMe (Int, String, Float)))
      it "Applicative" $
        property (quickBatch $ applicative (undefined :: CountMe (Int, String, Float)))
      it "Monad" $
        property (quickBatch $ monad (undefined :: CountMe (Int, String, Float)))
    describe "Nope" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: Nope (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $ applicative (undefined :: Nope (Int, Float, String)))
      it "Monad" $
        property (quickBatch $ monad (undefined :: Nope (Int, Float, String)))
    describe "PhbtEither" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: PhbtEither String (Int, Float, String)))
      it "Applicative" $
        property (quickBatch $ applicative (undefined :: PhbtEither String (Int, Float, String)))
      it "Monad" $
        property (quickBatch $ monad (undefined :: PhbtEither String (Int, Float, String)))
    describe "Identity" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: Identity (Int, String, Float)))
      it "Applicative" $
        property (quickBatch $ applicative (undefined :: Identity (Int, String, Float)))
      it "Monad" $
        property (quickBatch $ monad (undefined :: Identity (Int, String, Float)))
    describe "List" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: List (Int, String, Float)))
      it "Applicative" $
        property (quickBatch $ applicative (undefined :: List (Int, String, Float)))
      it "Monad" $
        property (quickBatch $ monad (undefined :: List (Int, String, Float)))
