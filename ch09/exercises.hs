import           Data.Bool
import           Data.Char
------------------------------------------------------------------
--Extracting portions of lists
------------------------------------------------------------------
--1.
myWords :: String -> [String]
myWords []       = []
myWords (' ':xs) = myWords xs
myWords xs       = takeWhile (/=' ') xs : myWords (dropWhile (/=' ') xs)
--2.
--PeomLines.hs
--3.
myBreak :: Char -> String -> [String]
myBreak c []     = []
myBreak c (x:xs)
  | x /= c = takeWhile (/= c) (x:xs) : myBreak c (dropWhile (/= c) (x:xs))
  | otherwise = myBreak c xs
------------------------------------------------------------------
--List comprehension exercises
------------------------------------------------------------------
mySqr = [ x^2 | x <- [1..5] ]
myCube = [y^3 | y <- [1..5]]

l1 = [x | x <- mySqr, rem x 2 == 0] -- even squares [4,16]
l2 = [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50] -- []
l3 = take 5 [ (x, y) | x <- mySqr
            , y <- mySqr, x < 50, y > 50 ] -- []

--
l4 = [ (x,y) | x <- mySqr, y <- myCube ]
l5 = [ (x,y) | x <- mySqr, y <- myCube, x < 50, y < 50 ]
l6 = length l5
------------------------------------------------------------------
--Spines and non-strict evaluation
------------------------------------------------------------------
--Will it blow up?
b1 = [x^y | x <- [1..5], y <- [2, undefined]] -- ⊥
b2 = take 1 $ [x^y | x <- [1..5], y <- [2, undefined]] -- [1]
b3 = sum [1, undefined, 3] -- ⊥
b4 = length [1, 2, undefined] -- 3
b5 = length $ [1,2,3] ++ undefined -- ⊥
b6 = take 1 $ filter even [1, 2, 3, undefined] -- [2]
b7 = take 1 $ filter even [1, 3, undefined] -- ⊥
b8 = take 1 $ filter odd [1, 3, undefined] -- [1]
b9 = take 2 $ filter odd [1, 3, undefined] -- [1, 3]
b0 = take 3 $ filter odd [1, 3, undefined] -- ⊥
------------------------------------------------------------------
--Transforming lists of values
------------------------------------------------------------------
--1.
bl1 = take 1 $ map (+1) [undefined, 2, 3] -- ⊥
--2.
bl2 = take 1 $ map (+1) [1, undefined, 2, 3] -- ⊥
--3.
bl3 = take 2 $ map (+1) [1, undefined, 2, 3] -- ⊥
--4.
itIsMystery :: String -> [Bool]
itIsMystery xs = map (\x -> elem x "aeiou") xs
--5.
m1 = map (^2) [1..10] -- [1^2, 2^2, .. , 10^2]
m2 = map minimum [[1..10], [10..20], [20..30]] -- [1, 10, 20]
m3 = map sum [[1..5], [1..5], [1..5]] -- [15, 15, 15]
--6.
myBool :: (Num a, Eq a) => a -> [a] -> [a]
myBool n xs = map (\x -> bool x (-x) (x == n)) xs
------------------------------------------------------------------
--Filtering lists of values
------------------------------------------------------------------
--1.
myFilter = filter (\x -> (rem x 3) == 0)
--2.
myF = length . myFilter
--3.
myFilter' = filter (\x -> x /= "the" && x /= "a" && x /= "an") . words
------------------------------------------------------------------
--Zipping exercises
------------------------------------------------------------------
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) =
  (x, y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] _ = []
myZipWith f _ [] = []
myZipWith f (x:xs) (y:ys) =
  (f x y) : myZipWith f xs ys

myZip' :: [a] -> [b] -> [(a, b)]
myZip' = myZipWith (,)
------------------------------------------------------------------
--Chapter Exercises
------------------------------------------------------------------
--2.
myFiltU = filter isUpper
--3.
myCap []     = []
myCap (x:xs) = toUpper x : xs
--4.
myCapR []     = []
myCapR (x:xs) = toUpper x : myCapR xs
--5.
test xs = toUpper $ head xs
--6.
testC xs = (toUpper . head) xs
testP = toUpper . head

--1.
myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs
--2.
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f
--3.
myElem :: Eq a => a -> [a] -> Bool
myElem n []     = False
myElem n (x:xs) = if x == n then True else myElem n xs

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny n = myAny (\x -> x == n)
--4.
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]
--5.
squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs
--6.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f []     = []
squishMap f (x:xs) = f x ++ squishMap f xs
--7.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id
--8.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:[]) = x
myMaximumBy f (x:y:xs) = if f x y == GT then myMaximumBy f (x:xs) else myMaximumBy f (y:xs)
--9.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:[]) = x
myMinimumBy f (x:y:xs) = if f x y == LT then myMinimumBy f (x:xs) else myMinimumBy f (y:xs)
--10.
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare
--11.
myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
