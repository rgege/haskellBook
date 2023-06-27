import           Data.Monoid

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem n = getAny . foldMap (Any . (==n))

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr f Nothing
  where
    f x Nothing  = Just x
    f x (Just y) = Just (min x y)

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr f Nothing
  where
    f x Nothing  = Just x
    f x (Just y) = Just (max x y)

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: (Foldable t) => t a -> Int
length' = foldr (\a b -> b + 1) 0

length'' :: (Foldable t) => t a -> Int
length'' = getSum . foldMap (const $ Sum 1)

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (\a b -> a : b) []

toList'' :: (Foldable t) => t a -> [a]
toList'' = foldMap pure

myfold :: (Foldable t, Monoid m) => t m -> m
myfold = foldMap (\a -> a <> mempty)

myfoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
myfoldMap f = foldr (\a b -> (f a) <> b) mempty

data Constant b a = Constant a

instance Foldable (Constant b) where
  foldr f z (Constant x) = f x z

  foldMap f (Constant x) = f x

data Two a b = Two a b

instance Foldable (Two a) where
  foldr f z (Two _ y) = f y z

  foldMap f (Two _ y) = f y

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldr f z (Three _ _ k) = f k z

  foldMap f (Three _ _ k) = f k

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ y y') = f y <> f y'

data Four a b = Four a b b b

instance Foldable (Four a) where
  foldMap f (Four _ y y' y'') = f y <> f y' <> f y''
