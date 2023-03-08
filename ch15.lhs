
> import Data.Monoid
> import Test.QuickCheck

> monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
> monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

> monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
> monoidLeftIdentity a = (mempty <> a) == a

> monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
> monoidRightIdentity a = (a <> mempty) == a

> data Optional a =
>     Nada
>   | Only a
>   deriving (Eq, Show)

> instance Monoid a => Monoid (Optional a) where
>   mempty = Nada
>   mappend = (<>)

> instance Semigroup a => Semigroup (Optional a) where
>   (<>) Nada     Nada      = Nada
>   (<>) (Only a) Nada      = Only a
>   (<>) Nada     (Only a)  = Only a
>   (<>) (Only a) (Only a') = Only (a <> a') 

> instance Arbitrary a => Arbitrary (Optional a) where
>   arbitrary = oneof 
>                 [ Only <$> arbitrary
>                 , return Nada
>                 ]
>

> newtype First' a =
>   First' { getFirst' :: Optional a }
>   deriving (Eq, Show)

> instance Monoid a => Monoid (First' a) where
>   mempty = First' Nada
>   mappend = (<>)

> instance Semigroup a => Semigroup (First' a) where
>   (<>) (First' (Only a)) (First' Nada)      = First' (Only a)
>   (<>) (First' Nada) (First' (Only a))      = First' (Only a)
>   (<>) (First' (Only a)) (First' (Only a')) = First' (Only (a <> a'))
>   (<>) (First' Nada) (First' Nada)          = First' Nada

> firstMappend :: Monoid a => First' a -> First' a -> First' a
> firstMappend  = mappend 

> type FirstMappend =
>      First' String
>   -> First' String
>   -> First' String
>   -> Bool

> instance Arbitrary a => Arbitrary (First' a) where
>   arbitrary = First' <$> arbitrary


-------------------------------------------------------------------------------- 
Chapter Exercises
-------------------------------------------------------------------------------- 
1.

> data Trivial = Trivial deriving (Eq, Show)

> instance Semigroup Trivial where
>   (<>) _ _ = Trivial

> instance Arbitrary Trivial where
>   arbitrary = return Trivial

> semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
> semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

> type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

2. 

> newtype Identity a = Identity a deriving (Eq, Show)

> instance Semigroup a => Semigroup (Identity a) where
>   (<>) (Identity a) (Identity a') = Identity (a <> a')

> instance Arbitrary a => Arbitrary (Identity a) where
>   arbitrary = Identity <$> arbitrary 

> type ArbitraryAssoc = 
>          Identity String 
>       -> Identity String  
>       -> Identity String 
>       -> Bool

3.

> data Two a b = Two a b deriving (Eq, Show)

> instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
>   (<>) (Two a b) (Two a' b') = Two (a <> a') (b <> b')

> instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
>   arbitrary = Two <$> arbitrary <*> arbitrary

> type TwoAssoc = 
>          Two String String
>       -> Two String String 
>       -> Two String String
>       -> Bool

> main :: IO ()
> main = do
>    quickCheck (monoidAssoc :: FirstMappend)
>    quickCheck (monoidLeftIdentity :: First' String -> Bool)
>    quickCheck (monoidRightIdentity :: First' String -> Bool)
>    quickCheck (semigroupAssoc :: TrivialAssoc)
>    quickCheck (semigroupAssoc :: ArbitraryAssoc)
>    quickCheck (semigroupAssoc :: TwoAssoc)
