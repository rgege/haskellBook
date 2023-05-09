module Main where

import           Data.Monoid
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import           Exercises                (List)

main :: IO ()
main =
  hspec $ do
    describe "List" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: List (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $ applicative (undefined :: List (Int, Float, String)))
