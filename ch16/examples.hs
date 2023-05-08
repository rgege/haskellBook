-------------------------------------------------------------------------------
-- Maybe
-------------------------------------------------------------------------------
incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just (n + 1)
incIfJust Nothing  = Nothing

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe = fmap (+1)

-- rewriting as more generic functions (more polymorphic)
liftedInc :: (Functor f, Num a) => f a -> f a
liftedInc = fmap (+1)

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust (Just s) = Just (show s)
showIfJust Nothing  = Nothing

showMaybe :: Show a => Maybe a -> Maybe String
showMaybe = fmap show

-- rewriting as more generic functions (more polymorphic)
liftedShowM :: (Functor f, Show a) => f a -> f String
liftedShowM = fmap show

-------------------------------------------------------------------------------
-- Either
-------------------------------------------------------------------------------

incIfRight :: Num a => Either e a -> Either e a
incIfRight (Right n) = Right (n + 1)
incIfRight (Left e)  = Left e

incEither :: Num a => Either e a -> Either e a
incEither = fmap (+1)

-- rewriting as more generic functions (more polymorphic)
liftedEither :: (Functor f, Num a) => f a -> f a
liftedEither = fmap (+1)

showIfRight :: Show a => Either e a -> Either e String
showIfRight (Right n) = Right (show n)
showIfRight (Left e)  = Left e

showEither :: Show a => Either e a -> Either e String
showEither = fmap show

-- rewriting as more generic functions (more polymorphic)
liftedShowE :: (Functor f, Show a) => f a -> f String
liftedShowE = fmap show

-------------------------------------------------------------------------------
-- More structure
-------------------------------------------------------------------------------
data Constant a b =
  Constant {getConstat :: a}
  deriving (Eq, Show)

instance Functor (Constant v) where
  fmap _ (Constant m) = Constant m

data Wrap f a =
  Wrap (f a)
  deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

-------------------------------------------------------------------------------
-- IO Functor
-------------------------------------------------------------------------------
getInt :: IO Int
getInt = fmap read getLine

data Tuple a b =
  Tuple a b
  deriving (Eq, Show)

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Tuple a) where
  fmap f (Tuple a b) = Tuple a (f b)

instance Functor (Flip Tuple a) where
  fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b
