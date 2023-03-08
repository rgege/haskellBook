> import Control.Monad
> import Data.Monoid
> import Test.QuickCheck

-------------------------------------------------------------------------------- 
Chapter 15. Monoid, Semigroup
-------------------------------------------------------------------------------- 

> data Booly a =
>     False'
>   | True'
>   deriving (Eq,Show)

> instance Semigroup (Booly a) where
>   (<>) False' _      = False'
>   (<>) _      False' = False' 
>   (<>) True'  True'  = True'

-------------------------------------------------------------------------------- 
Exercise
-------------------------------------------------------------------------------- 

> data Optional a =
>     Nada
>   | Only a
>   deriving (Eq, Show)

> instance Monoid a => Monoid (Optional a) where
>   mempty = Nada

> instance Semigroup a => Semigroup (Optional a) where
>   (<>) (Only a) (Only a') = Only (a <> a')
>   (<>) (Only a) Nada      = Only a
>   (<>) Nada     (Only a)  = Only a
>   (<>) Nada     Nada      = Nada

> newtype First' a =
>   First' { getFirst' :: Optional a }
>   deriving (Eq, Show)

> instance Monoid (First' a) where
>   mempty = First' Nada
>   mappend = (<>)

> instance Semigroup (First' a) where
>   First' (Only a) <> First' (Only a') = First' (Only a)
>   First' (Only a) <> First' Nada      = First' (Only a)
>   First' Nada     <> First' (Only a)  = First' (Only a)
>   First' Nada     <> (First' Nada)    = First' Nada

> firstMappend :: First' a -> First' a -> First' a
> firstMappend = mappend

> type FirstMappend =
>   First' String
>   -> First' String
>   -> First' String
>   -> Bool

> monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
> monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

> monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
> monoidLeftIdentity a = (mempty <> a) == a

> monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
> monoidRightIdentity a = (a <> mempty) == a

> instance Arbitrary a => Arbitrary (First' a)
>    where arbitrary = oneof
>            [ pure $ First' Nada
>            , First' . Only <$> arbitrary ]

> main :: IO ()
> main = do
>   quickCheck (monoidAssoc :: FirstMappend)
>   quickCheck (monoidLeftIdentity :: First' String -> Bool)
>   quickCheck (monoidRightIdentity :: First' String -> Bool)
