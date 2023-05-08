import           Data.List
------------------------------------------------------------------
--Short Exercises
------------------------------------------------------------------
--1.
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])
--2.
y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z
--3.
x :: Maybe Int
x = elemIndex 3 [1,2,3,4,5]

y' :: Maybe Int
y' = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x  <*> y'
--4.
xs = [1,2,3]
ys = [4,5,6]

k :: Maybe Integer
k = lookup 3 $ zip xs ys

j :: Maybe Integer
j = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = (<$>) sum $ (,) <$> k <*> j
------------------------------------------------------------------
--Write an Applicative instance for Identity
------------------------------------------------------------------
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap g (Identity a)  = Identity (g a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity x) (Identity y) = Identity (x y)
------------------------------------------------------------------
--Write an Applicative instance for Constant
------------------------------------------------------------------
newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>)  (Constant x) (Constant y) = Constant (x <> y)
------------------------------------------------------------------
--Exercise
------------------------------------------------------------------
--1.
work1 = const <$> Just "Hello" <*> pure "World"
--2.
work2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]
------------------------------------------------------------------
--Implement the List Applicative
------------------------------------------------------------------
