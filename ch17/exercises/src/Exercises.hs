module Exercises where

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
------------------------------------------------------------------
--Write the Either Applicative that short-circuits on any error
--values
------------------------------------------------------------------
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

data Validation e a =
    Error e
  | Success' a
  deriving (Eq, Show)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = frequency [(1, Error <$> arbitrary),
                         (2, Success' <$> arbitrary)]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

instance Functor (Sum a) where
  fmap _ (First a)  = First a
  fmap g (Second b) = Second $ g b

instance Applicative (Sum a) where
  pure = Second
  (<*>) (First a)  _          = First a
  (<*>) _ (First a)           = First a
  (<*>) (Second f) (Second a) = Second $ f a

instance Functor (Validation e) where
  fmap _ (Error e)    = Error e
  fmap g (Success' a) = Success' $ g a

instance Monoid e => Applicative (Validation e) where
  pure = Success'
  (<*>) (Error x) (Error y)       = Error (x <> y)
  (<*>) (Error x) _               = Error x
  (<*>) _ (Error x)               = Error x
  (<*>) (Success' g) (Success' a) = Success' $ g a
------------------------------------------------------------------
--Write applicative instances for the following datatypes
------------------------------------------------------------------
--1.
newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity $ f a
--2.
data Pair a = Pair a a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    l <- arbitrary
    r <- arbitrary
    return $ Pair l r

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

instance Functor Pair where
  fmap g (Pair l r) = Pair (g l) (g r)

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f g) (Pair l r) = Pair (f l) (g r)
--3.
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance Functor (Two a) where
  fmap g (Two a b) = Two a $ g b

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two c g) (Two a b) = Two (c <> a) $ g b
--4.
data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance Functor (Three a b) where
  fmap g (Three a b c) = Three a b $ g c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three x y g) (Three a b c) = Three (x <> a) (y <> b) $ g c
--4.
data Three' a b  = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    l <- arbitrary
    r <- arbitrary
    return $ Three' a l r

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance Functor (Three' a)  where
  fmap g (Three' a l r) = Three' a (g l) (g r)

instance Monoid a => Applicative (Three' a) where
  pure a = Three' mempty a a
  (<*>) (Three' c f g) (Three' a l r) = Three' (c <> a) (f l) (g r)
--5.
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
          => Arbitrary (Four a b c d) where
            arbitrary = do
              a <- arbitrary
              b <- arbitrary
              c <- arbitrary
              d <- arbitrary
              return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

instance Functor (Four a b c) where
  fmap g (Four a b c d) = Four a b c $ g d

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (<*>) (Four x y z g) (Four a b c d) = Four (x <> a) (y <> b) (z <> c) $ g d
--7.
data Four' a b = Four' a a a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    l <- arbitrary
    c <- arbitrary
    r <- arbitrary
    b <- arbitrary
    return $ Four' l c r b

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

instance Functor (Four' a) where
  fmap g (Four' l r c b) = Four' l r c $ g b

instance Monoid a => Applicative (Four' a) where
  pure a = Four' mempty mempty mempty a
  (<*>) (Four' l c r g) (Four' a a' a'' b) =
    Four' (l <> a) (c <> a') (r <> a'') $ g b


