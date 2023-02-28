
> module Jammin where

> import Data.Int
> import Data.List

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
>   Jam { fruit :: Fruit 
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
