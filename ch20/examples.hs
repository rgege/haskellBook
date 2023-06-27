data Identity a = Identity a

instance Foldable Identity where
  foldr f z (Identity x) = f x z

  foldMap f (Identity x) = f x

data Optional a =
    Yep a
  | Nada
  deriving (Eq, Show)

instance Functor Optional where
  fmap f (Yep x) = Yep $ f x
  fmap _ Nada    = Nada

instance Foldable Optional where
  foldr _ z Nada    = z
  foldr f z (Yep x) = f x z

  foldMap f (Yep x) = f x
  foldMap _ Nada    = mempty

null' :: Foldable t => t a -> Bool
null' = undefined
