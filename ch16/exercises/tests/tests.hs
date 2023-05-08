module Main where

{-# LANGUAGE ViewPatterns #-}

import           Test.Hspec
import           Test.QuickCheck

import           Exercises       (Identity, Pair, Two, Three)


--------------------------------------------------------------------------------
-- Functor Laws
--------------------------------------------------------------------------------

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => Fun a b -> Fun b c -> f a -> Bool
functorCompose (Fun _ f) (Fun _ g) x = (fmap g (fmap f x)) == (fmap (g . f) x)

type FId1 f a = f a -> Bool

type FId2 f a b = f a b -> Bool

type FId3 f a b c = f a b c -> Bool

type FComp1 f a b c = Fun a b -> Fun b c -> f a -> Bool

type FComp2 f x a b c = Fun a b -> Fun b c -> f x a -> Bool

type FComp3 f x y a b c = Fun a b -> Fun b c -> f x y a -> Bool



main :: IO ()
main =
  hspec $ do
    describe "Identity" $ do
      it "Identity" $ property (functorIdentity :: FId1 Identity String)
      it "Compose"  $ property (functorCompose :: FComp1 Identity String Float Int)
    describe "Pair" $ do
      it "Identity" $ property (functorIdentity :: FId1 Pair String)
      it "Compose" $  property (functorCompose :: FComp1 Pair Float String Int)
    describe "Two" $ do
      it "Identity" $ property (functorIdentity :: FId2 Two String Int)
      it "Compose"  $  property (functorCompose :: FComp2 Two Float Double String Int)
    describe "Three" $ do
      it "Identity" $ property (functorIdentity :: FId3 Three String Int Double)
      it "Compose"  $  property (functorCompose :: FComp3 Three Float Double String Int Integer)
