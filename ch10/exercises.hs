------------------------------------------------------------------
--Chapter Exercises
------------------------------------------------------------------

-- Rewriting functions using folds
-- 1.
myOr :: [Bool] -> Bool
myOr = foldr (||) False
-- 2.
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f
-- 3.
myElemR :: Eq a => a -> [a] -> Bool
myElemR a [] = False
myElemR a (x:xs)
  | x == a = True
  | otherwise = myElemR a xs

myElemF :: Eq a => a -> [a] -> Bool
myElemF n = foldr (\a b -> b || a == n) False

myElem :: Eq a => a -> [a] -> Bool
myElem n = any (==n)
-- 4.
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []
-- 5.
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) []
-- 6.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a == True then a : b else b) []
-- 7.
squish :: [[a]] -> [a]
squish = foldr (++) []
-- 8.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []
-- 9.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id
-- 10.
myMaximumBy :: Num a => (a -> a -> Ordering) -> [a] -> a
myMaximumBy f = foldl (\a b -> if (f a b) == GT then a else b) 1
-- 11.
myMinimumBy :: Num a => (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = foldl (\a b -> if (f a b) == LT then a else b) 1
