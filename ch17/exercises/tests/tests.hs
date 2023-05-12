module Main where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import           Exercises

main :: IO ()
main =
  hspec $ do
    describe "List" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: List (Int, Float, String)))
      it "Applicative" $
        property
           (quickBatch $ applicative (undefined :: List (Int, Float, String)))
    describe "Validation" $ do
      it "Functor" $
        property
          (quickBatch $
           functor (undefined :: Validation String (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $
           applicative (undefined :: Validation String (Int, Float, String)))
    describe "Identity" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: Identity (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $
           applicative (undefined :: Identity (Int, Float, String)))
    describe "Pair" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: Pair (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $
           applicative (undefined :: Pair (Int, Float, String)))
    describe "Two" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: Two String (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $
           applicative (undefined :: Two String (Int, Float, String)))
    describe "Three" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: Three String String (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $
        applicative (undefined :: Three String String (Int, Float, String)))
    describe "Three'" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: Three' String (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $
        applicative (undefined :: Three' String (Int, Float, String)))
    describe "Four" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: Four String String String (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $
        applicative (undefined :: Four String String String (Int, Float, String)))
    describe "Four'" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: Four' String (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $
        applicative (undefined :: Four' String (Int, Float, String)))
