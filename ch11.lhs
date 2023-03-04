
> module Jammin where

> import Data.Int
> import Data.List
> import Data.Char

-------------------------------------------------------------------------------- 
Intermission: Exercises p. 396
-------------------------------------------------------------------------------- 

> class TooMany a where
>   tooMany :: a -> Bool

> instance TooMany Int where
>   tooMany n = n > 45

> newtype Goat = Goat Int deriving (Show, TooMany)

1. Write an instance of the typeclass for the type (Int, String).

> instance TooMany (Int, String) where
>   tooMany (i, _) = i > 45

> newtype Lamb = Lamb (Int, String) deriving (Show, TooMany)

2. Make another TooMany instance for (Int, Int).

 instance TooMany (Int, Int) where
   tooMany (i, i') = (i + i') > 100

> newtype Lambs = Lambs (Int, Int) deriving (Show, TooMany)

3. Make another TooMany instance for (Num a, TooMany a) => (a, a).

> instance (Num a, TooMany a) => TooMany (a, a) where
>   tooMany (x, y) = True

-------------------------------------------------------------------------------- 
Intermission: Exercises p. 398
-------------------------------------------------------------------------------- 
1. Given:

> data BigSmall = 
>          Big Bool
>        | Small Bool 
>          deriving (Eq, Show)

What is the cardinality of this datatype? 
(2+2)

2. Given: 

> data NumberOrBool = 
>         Numba Int8
>       | BoolyBool Bool
>         deriving (Eq, Show)

What is the cardinality of NumberOrBool? 
(128+127+1+2)

-------------------------------------------------------------------------------- 
Intermission: Jammin Exercises p. 402
-------------------------------------------------------------------------------- 

> data Fruit = 
>         Peach
>       | Plum
>       | Apple
>       | Blackberry
>         deriving (Ord, Eq, Show)

 data  JamJars =
   Jam Fruit Int
   deriving (Ord, Eq, Show)
      
1. Let’s make a module for this.

2. Rewrite JamJars with record syntax.

> data JamJars = 
>   Jam {fruit :: Fruit
>       , jars :: Int} 
>         deriving (Ord, Eq, Show)

3. What is the cardinality of JamJars?
(4 * (cardinality of Int)) = (4 * 2^64)

4. Add Ord instances to your deriving clauses.

5. You can use the record ﬁeld accessors in other functions as well.

> row1 = Jam Peach 10
> row2 = Jam Blackberry 35
> row3 = Jam Apple 99
> row4 = Jam Blackberry 15
> row5 = Jam Plum 32

> allJam = [row1, row2, row3, row4, row5]

6. Write a function that will return the total number of jars of jam.

> totalJars :: [JamJars] -> Int
> totalJars = sum . map jars 

7. Write a function that will tell you which row has the most jars of jam in it.

> compareCount :: JamJars -> JamJars -> Ordering
> compareCount (Jam _ c) (Jam _ c') = compare c c'

> mostRow :: [JamJars] -> JamJars
> mostRow = foldr1 (\a b -> if compareCount a b == GT then a else b)

8. Under your module name, import the module called Data.List.

9. You’ll want to sort the list allJams by the ﬁrst ﬁeld in each record.

> compareKind :: JamJars -> JamJars -> Ordering
> compareKind (Jam k _) (Jam k' _) = compare k k'

> sortJam :: [JamJars] -> [JamJars]
> sortJam = sortBy compareKind

> groupJam :: [JamJars] -> [[JamJars]]
> groupJam = groupBy (\a b -> compareKind a b == EQ) . sortJam

-------------------------------------------------------------------------------- 
Intermission: Exercises p. 406
-------------------------------------------------------------------------------- 
1. Given

 data FlowerType = 
     Gardenia
   | Daisy
   | Rose
   | Lilac
     deriving Show

> type Gardener = String

 data Garden =
   Garden Gardener FlowerType
   deriving Show

What is the normal form of Garden?

> data Garden = 
>     Gardenia Gardener
>   | Daisy Gardener
>   | Rose Gardener
>   | Lilac Gardener
>     deriving Show

-------------------------------------------------------------------------------- 
Intermission: Exercises p. 423
--------------------------------------------------------------------------------
1. Determine how many unique inhabitants each type has.

> data Quad =
>     One
>   | Two
>   | Three
>   | Four
>     deriving Show

eQuad :: Either Quad Quad
(4+4)

prodQuad :: (Quad, Quad)
(4*4)

funcQuad :: Quad -> Quad
(4^4)

prodTBool :: (Bool, Bool, Bool)
2*2*2

gTwo :: Bool -> Bool -> Bool
2^(2*2)

fTwo :: Bool -> Quad -> Quad
4^(4*2)

-------------------------------------------------------------------------------- 
Write map for BinaryTree
--------------------------------------------------------------------------------

> data BinaryTree a =
>     Leaf
>   | Node (BinaryTree a) a (BinaryTree a)
>     deriving (Eq, Ord, Show)

> mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
> mapTree _ Leaf = Leaf
> mapTree f (Node left a right) = 
>  (Node 
>   (mapTree f left) 
>     (f a) 
>       (mapTree f right))

> testTree' :: BinaryTree Integer
> testTree' =
>   Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

> mapExpected =
>   Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

> mapOkay =
>   if mapTree (+1) testTree' == mapExpected
>   then print "yup okay!"
>   else error "test failed!"

-------------------------------------------------------------------------------- 
Convert binary trees to lists
--------------------------------------------------------------------------------

> preorder :: BinaryTree a -> [a]
> preorder Leaf = []
> preorder (Node left a right) = a : preorder left ++ preorder right

> inorder :: BinaryTree a -> [a]
> inorder Leaf = []
> inorder (Node left a right) = preorder left ++ [a] ++ preorder right

> postorder :: BinaryTree a -> [a]
> postorder Leaf = []
> postorder (Node left a right) = postorder left ++ postorder right ++ [a] 

> testTree :: BinaryTree Integer
> testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

> testPreorder :: IO ()
> testPreorder =
>   if preorder testTree == [2, 1, 3]
>   then putStrLn "Preorder fine!"
>   else putStrLn "Bad news bears."

> testInorder :: IO ()
> testInorder =
>   if inorder testTree == [1, 2, 3]
>   then putStrLn "Inorder fine!"
>   else putStrLn "Bad news bears."

> testPostorder :: IO ()
> testPostorder =
>   if postorder testTree == [1, 3, 2]
>   then putStrLn "Postorder fine!"
>   else putStrLn "postorder failed check"

 main :: IO ()
 main = do
   testPreorder
   testInorder
   testPostorder

-------------------------------------------------------------------------------- 
Write foldr for BinaryTree
--------------------------------------------------------------------------------

> foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
> foldTree f acc Leaf = acc
> foldTree f acc (Node left a right) = f a (foldTree f (foldTree f acc left) right)


--------------------------------------------------------------------------------
Write foldr for BinaryTree
--------------------------------------------------------------------------------

> mapTree' :: (a -> b) -> BinaryTree a -> BinaryTree b
> mapTree' f bt = foldTree (\a acc -> Node acc (f a) Leaf) Leaf bt

--------------------------------------------------------------------------------
Chapter Exercises
--------------------------------------------------------------------------------
1. Given the following datatype:
 data Weekday =
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday

a) Weekday is a type with ﬁve data constructors

2. f Friday -> "Miller Time" 
   f :: Weekday -> String

3. Types deﬁned with the data keyword
b) must begin with a capital letter

4. The function g xs = xs !! (length xs - 1)
c) delivers the ﬁnal element of xs

--------------------------------------------------------------------------------
Vigene Cipher
--------------------------------------------------------------------------------

> data Secret = 
>    Secret { phrase :: String
>           , keyword :: String}
>           deriving Show

> kwrdStr :: Secret -> String
> kwrdStr s = concat $ replicate (length (phrase s)) (map toUpper (keyword s))

> kwrd :: [Char] -> [Char] -> [Char]
> kwrd [] _ = []
> kwrd (x:xs) (y:ys)
>   | isSpace x = x : kwrd xs (y:ys)
>   | otherwise = y : kwrd xs ys

> helper :: Int -> Int
> helper n
>   | n == 32   = 0
>   | otherwise = n - 65

> corrector :: Int -> Int
> corrector n
>   | n == 32   = 32
>   | n < 91    = n
>   | otherwise = (mod n 91) + 65

> phrsLst :: Secret -> [Int]
> phrsLst s = map ord (map toUpper (phrase s))

> kwrdLst :: Secret -> [Int]
> kwrdLst s = map (\x -> helper x) $ map ord $ kwrd (phrase s) (kwrdStr s)

> cipher :: Secret -> [Char]
> cipher s = map chr $ 
>             map (\x -> corrector x) $ 
>             zipWith (+) (phrsLst s) (kwrdLst s) 

--------------------------------------------------------------------------------
As-patterns
--------------------------------------------------------------------------------

> f :: Show a => (a, b) -> IO (a, b)
> f t@(a, _) = do
>  print a
>  return t

> doubleUp :: [a] -> [a]
> doubleUp []       = []
> doubleUp xs@(x:_) = x : xs

1.

> isSubsequenceOf' :: (Eq a) => [a] -> [a] -> Bool
> isSubsequenceOf' [] _ = True
> isSubsequenceOf' _ [] = False
> isSubsequenceOf' xss@(x:xs) yss@(y:ys)
>   | x == y    = isSubsequenceOf' xs yss
>   | otherwise = isSubsequenceOf' xss ys

> testisSubsequenceOf1 :: IO ()
> testisSubsequenceOf1 =
>   if isSubsequenceOf' "blah" "blahwoot" 
>   then putStrLn "fine!"
>   else putStrLn "failed check"

> testisSubsequenceOf2 :: IO ()
> testisSubsequenceOf2 =
>   if isSubsequenceOf' "blah" "wootblah"  
>   then putStrLn "fine!"
>   else putStrLn "failed check"

> testisSubsequenceOf3 :: IO ()
> testisSubsequenceOf3 =
>   if isSubsequenceOf' "blah" "wwotblah"
>   then putStrLn "fine!"
>   else putStrLn "failed check"

> testisSubsequenceOf4 :: IO ()
> testisSubsequenceOf4 =
>   if isSubsequenceOf' "blah" "wootbla" == False
>   then putStrLn "fine!"
>   else putStrLn "failed check"

> main :: IO ()
> main = do
>   testisSubsequenceOf1 
>   testisSubsequenceOf2 
>   testisSubsequenceOf3
>   testisSubsequenceOf4 

2. 

> myWords :: String -> [String]
> myWords [] = []
> myWords (' ':ss) = myWords ss
> myWords ss = takeWhile (/=' ') ss : myWords 
>                      (dropWhile (/=' ') ss)

> capitalizeWords :: String -> [(String, String)]
> capitalizeWords = map cap . myWords 
>     where cap xss@(x:xs) = (,) xss ((toUpper x) : xs)

--------------------------------------------------------------------------------
Langauge Exercises
--------------------------------------------------------------------------------
1. Write a function that capitalizes a word.

> capitalizeWord :: String -> String
> capitalizeWord [] = []
> capitalizeWord (x:xs) = toUpper x : xs

2. Write a function that capitalizes sentences in a paragraph.

> splitParag []       = []
> splitParag ('.':ss) = splitParag ss
> splitParag (' ':ss) = splitParag ss
> splitParag ss = takeWhile (/='.') ss : splitParag (dropWhile (/='.') ss)

> capitalizeParagraph :: String -> String
> capitalizeParagraph = unwords. map capP . splitParag
>     where capP (x:xs) = toUpper x : xs ++ "."

--------------------------------------------------------------------------------
 PhoneExercise
--------------------------------------------------------------------------------

> data DaPhone = DaPhone [(Char, String)] deriving Show

> encoding = DaPhone
>          [
>            ('2', "abc2")
>          , ('3', "def3")
>          , ('4', "ghi4")
>          , ('5', "jkl5")
>          , ('6', "mno6")
>          , ('7', "pqrs7")
>          , ('8', "tuv8")
>          , ('9', "wxyz9")
>          , ('0', "+ ")
>          , ('#', ".,")
>          , ('*', "^")
>          ]

> type Presses = Int
> type Digit = Char

> getPresses :: Char -> String -> Int
> getPresses c cs = case elemIndex c cs of
>   Just i  -> i + 1
>   Nothing -> 0

> onPhone :: Char -> DaPhone -> (Digit, Presses)
> onPhone c (DaPhone xs)
>   | elem c (snd (head xs)) = (,) (fst (head xs)) (getPresses c (snd (head xs)))
>   | otherwise = onPhone c (DaPhone (tail xs))

> reverseTaps :: Char -> DaPhone -> [(Digit, Presses)]
> reverseTaps c (DaPhone xs)
>   | isUpper c = ('*', 1) : [onPhone (toLower c) (DaPhone xs)]
>   | otherwise = [onPhone c (DaPhone xs)]

> cellPhonesDead :: String -> DaPhone -> [(Digit, Presses)]
> cellPhonesDead [] _ = []
> cellPhonesDead (c:cs) (DaPhone xs) = reverseTaps c (DaPhone xs) ++ cellPhonesDead cs (DaPhone xs)

