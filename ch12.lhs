-------------------------------------------------------------------------------- 
Chapter 12. Signaling Adversity
-------------------------------------------------------------------------------- 

> ifEvenAddTwo :: Int -> Maybe Int
> ifEvenAddTwo n = if even n then Just (n+2) else Nothing

> type Name = String
> type Age  = Integer

> data Person = Person Name Age deriving Show

> mkPerson :: Name -> Age -> Maybe Person
> mkPerson name age
>   | name /= "" && age >= 0 = Just $ Person name age
>   | otherwise = Nothing

> data PersonInvalid = NameEmpty
>                    | AgeTooLow
>                    deriving (Show)

> blah :: PersonInvalid -> String
> blah pi
>   | pi == NameEmpty = "NameEmpty"
>   | pi == AgeTooLow = "AgeTooLow"
>   | otherwise = "???"

> instance Eq PersonInvalid where
>   (==) NameEmpty  NameEmpty = True
>   (==) AgeTooLow  AgeTooLow = True

> mkPerson' :: Name -> Age -> Either PersonInvalid Person
> mkPerson' name age
>   | name /= "" && age >= 0 = Right $ Person name age
>   | name == "" = Left NameEmpty
>   | otherwise = Left AgeTooLow

separate checking functions

> type ValidatePerson a = Either [PersonInvalid] a

> ageOkay :: Age -> Either [PersonInvalid] Age
> ageOkay age = case age >= 0 of
>   True -> Right age
>   False  -> Left [AgeTooLow]

> nameOkay :: Name -> Either [PersonInvalid] Name 
> nameOkay name = case name /= "" of
>   True  -> Right name
>   False -> Left [NameEmpty]

> mkPerson'' :: Name -> Age -> ValidatePerson Person
> mkPerson'' name age = mkPerson'''
>   (nameOkay name) (ageOkay age) 

> mkPerson''' :: ValidatePerson Name
>             -> ValidatePerson Age
>             -> ValidatePerson Person

> mkPerson''' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
> mkPerson''' (Left badName) (Left badAge) = Left (badName ++ badAge)
> mkPerson''' (Left badName) _             = Left (badName)
> mkPerson''' _              (Left badAge) = Left (badAge)

-------------------------------------------------------------------------------- 
Chapter Exercises
-------------------------------------------------------------------------------- 

-------------------------------------------------------------------------------- 
Determine the kinds
-------------------------------------------------------------------------------- 

1. Given id :: a -> a

What is the kind of a?
a :: *

2. r :: a -> f a

What are the kinds of a and f?
a ::  * ; f :: * -> *

-------------------------------------------------------------------------------- 
String processing
-------------------------------------------------------------------------------- 
1. Write a recursive function that takes a text/string, breaks it into
words and replaces each instance of ”the” with ”a”.

> notThe :: String -> Maybe String
> notThe s
>   | s == "the" = Nothing
>   | otherwise  = Just s

replaceThe :: String -> String

> replaceThe :: String -> String
> replaceThe = unwords . go . words . replace . notThe 
>   where 
>     replace Nothing   = "a"
>     replace (Just s)  =  s
>     go [] = []
>     go (x:xs)
>       | x == "the" = "a" : go xs
>       | otherwise  = x : go xs


2. Write a recursive function that takes a text/string, breaks it into
words, and counts the number of instances of ”the” followed by
a vowel-initial word.

> countBeforeVowel :: String -> Integer
> countBeforeVowel =  go . words
>   where 
>     go [] = 0
>     go (x:xs)
>       | x == "the" && elem (head (head xs)) vowels = 1 + go xs
>       | otherwise  = go xs    
>     vowels = "aeiou"

3. Return the number of letters that are vowels in a word.

> countVowels :: String -> Integer
> countVowels [] = 0
> countVowels (x:xs) 
>   | elem x vowels = 1 + countVowels xs
>   | otherwise     = countVowels xs
>   where
>     vowels = "aeiou"

-------------------------------------------------------------------------------- 
Validate the word
-------------------------------------------------------------------------------- 
1. Use the Maybe type to write a function that counts the number of
vowels in a string and the number of consonants. If the number
of vowels exceeds the number of consonants, the function returns
Nothing.

> newtype Word' = 
>   Word' String
>   deriving (Eq, Show)

> mkWord :: String -> Maybe Word'
> mkWord xs = if (go xs) > 0 then Nothing else Just (Word' xs)
>  where 
>     go [] = 0
>     go (x:xs)
>       | x == ' '      = go xs
>       | elem x vowels = 1 + go xs
>       | otherwise     = go xs - 1
>     vowels = "aeiou"

-------------------------------------------------------------------------------- 
It's only natural
-------------------------------------------------------------------------------- 
1. Implement functions to convert Naturals to Integers and Integers to Naturals.

> data Nat = 
>     Zero
>   | Succ Nat
>   deriving (Eq, Show)

> natToInteger :: Nat -> Integer
> natToInteger Zero = 0
> natToInteger (Succ a) = 1 + natToInteger a 

> integerToNat :: Integer -> Maybe Nat
> integerToNat n
>   | n < 0 = Nothing
>   | otherwise = Just $ conv n 
>   where
>     conv 0 = Zero
>     conv n = Succ $ conv (n-1)

-------------------------------------------------------------------------------- 
Small library for Maybe
-------------------------------------------------------------------------------- 
Write the following functions. This may take some time.

1. Simple boolean checks for Maybe values.

> isJust :: Maybe a -> Bool
> isJust (Just _) = True
> isJust Nothing  = False

> isNothing :: Maybe a -> Bool
> isNothing (Just _) = False
> isNothing Nothing = True

2. The following is the Maybe catamorphism. 
You can turn a Maybe value into anything else with this.

> mayybee :: b -> (a -> b) -> Maybe a -> b
> mayybee x _ Nothing = x
> mayybee x f (Just y) = f y

3. In case you just want to provide a fallback value.

> fromMaybe :: a -> Maybe a -> a
> fromMaybe x Nothing = x
> fromMaybe _ (Just y) = y 


4. Converting between List and Maybe.

> listToMaybe :: [a] -> Maybe a
> listToMaybe []     = Nothing
> listToMaybe (x:_) = Just x

> maybeToList :: Maybe a -> [a]
> maybeToList Nothing  = []
> maybeToList (Just x) = [x]  

5. For when we just want to drop the Nothing values from our list.

> catMaybes :: [Maybe a] -> [a]
> catMaybes [] = []
> catMaybes (Nothing:xs) = catMaybes xs
> catMaybes ((Just x):xs) = x : catMaybes xs

6. sequence

 flipMaybe :: [Maybe a] -> Maybe [a]

> flipMaybe xs = if (go xs) == "" then Nothing else Just (go xs)
>   where
>     go []            = []
>     go (Nothing:xs)  = ""
>     go ((Just x):xs) = x : go xs

-------------------------------------------------------------------------------- 
Small library for Either
-------------------------------------------------------------------------------- 
