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
