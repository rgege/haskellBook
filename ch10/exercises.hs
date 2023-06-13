------------------------------------------------------------------
--Intermission: Exercises
------------------------------------------------------------------
{-
1. b, c
2.
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc xs =
  case xs of
    []     -> acc
    (x:xs) -> foldl f (foldl f acc x) xs

foldl (flip (*)) [1..3]

foldl (foldl (flip (*)) ((flip (*)) 1 1) [2,3]
foldl (foldl (flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) [3]
foldl (foldl (flip (*)) ((flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) 3) []
((flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) 3)
((flip (*)) ((flip (*)) 1 2) 3)
((flip (*)) 2 3)
= 6
------------------------------------------------------------------
3. c
4. a
-}
--5.
--foldr (a -> b -> b) -> b -> [a] -> b
--foldl (b -> a -> b) -> b -> [a] -> b
f1 = foldr (++) "" ["woot", "WOOT", "woot"]
f2 = foldr max ' ' "fear is the little death"
f3 = foldr (&&) True [False, True]
f4 = foldr (||) False [False, True]
f5 = foldr ((++) . show) "" [1..5]
f6 = foldl const 'a' [1..5]
f7 = foldr (flip const) 0 "tacos"
ft = foldl const 0 "tacos"
f8 = foldr (flip const) 0 "burritos"
f9 = foldr (flip const) 'z' [1..5]
------------------------------------------------------------------
--Scans Exercises
------------------------------------------------------------------
fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x
--1.
fibs20 = take 20 fibs
--2.
fibs100 = takeWhile (<100) fibs
--3.
fact :: [Integer]
fact = scanl (*) 1 [1 .. ]

factN :: Integer -> Integer
factN x = fact !! fromIntegral x
------------------------------------------------------------------
--Warm-up and review
------------------------------------------------------------------
--1.
stops = "pbtdkg"
vowels = "aeiou"

sws  = [ (x, y, z) | x <- stops, y <- vowels, z <- stops ]
swsP = [ (x, y, z) | x <- stops, y <- vowels, z <- stops , x == 'p']

noun = ["lamp", "cellar", "leather", "belief", "zipper", "oranges", "existence", "sheet", "soap", "gate",
        "recess", "wing"]

verb = ["confess", "consider", "kiss", "behave", "tumble", "stuff", "mug", "fool", "surprise", "regret",
       "flood", "whip" ]

nvn = [ (x, y, z) | x <- noun, y <- verb, z <- noun ]

--2. calculates the average word length of the input string.
seekritFunc :: String -> Int
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))
--3.
seekritFuncPrec :: String -> Double
seekritFuncPrec x =
  (/) (fromIntegral (sum (map length (words x))))
      (fromIntegral (length (words x)))
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
-- foldr ((:) . f) []

-- 6.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a == True then a : b else b) []
-- 7.
squish :: [[a]] -> [a]
squish = foldr (++) []
-- 8.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []
-- foldr ((++) . f) []

-- 9.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id
-- 10.
myMaximumBy :: Num a => (a -> a -> Ordering) -> [a] -> a
myMaximumBy f = foldl (\a b -> if (f a b) == GT then a else b) 1
-- 11.
myMinimumBy :: Num a => (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = foldl (\a b -> if (f a b) == LT then a else b) 1
