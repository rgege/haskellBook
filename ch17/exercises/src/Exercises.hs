module Exercises
  (List) where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers

------------------------------------------------------------------
--Implement the List Applicative
------------------------------------------------------------------
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Ord, Show)

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil         = b
fold f b (Cons x xs) = f x (fold f b xs)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap _ Nil         = Nil
flatMap f (Cons x xs) = concat' $ fmap f (Cons x xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    frequency [ (1, return Nil)
              , (2, Cons <$> arbitrary <*> arbitrary) ]

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Functor List where
  fmap _ Nil         = Nil
  fmap g (Cons x xs) = Cons (g x) (fmap g xs)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) _ Nil          = Nil
  (<*>) Nil _          = Nil
  (<*>) (Cons x xs) ys = append (x <$> ys) (xs <*> ys)
------------------------------------------------------------------
--Implement the ZipList Applicative
------------------------------------------------------------------
take' :: Int -> List a -> List a
take' = undefined

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure = undefined
  (<*>) = undefined




