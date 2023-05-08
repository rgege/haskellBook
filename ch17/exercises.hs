data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil         = b
fold f b (Cons x xs) = f x (fold f b xs)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f Nil         = Nil
flatMap f (Cons x xs) = concat' $ fmap f (Cons x xs)

instance Functor List where
  fmap _ Nil         = Nil
  fmap g (Cons x xs) = Cons (g x) (fmap g xs)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) _ Nil          = Nil
  (<*>) Nil _          = Nil
  (<*>) (Cons g gs) ys = append (g <$> ys) (gs <*> ys)

