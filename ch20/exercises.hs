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

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (\a b -> a : b) []

myfold :: (Foldable t, Monoid m) => t m -> m
myfold = foldMap (\a -> a <> mempty)

myfoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
myfoldMap f = foldr (\a b -> (f a) <> b) mempty
