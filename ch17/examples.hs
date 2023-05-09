validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen
  then Nothing
  else Just s

newtype Name   = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person =
  Person Name Address
  deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  Person <$> mkName n <*> mkAddress a
------------------------------------------------------------------
data Cow = Cow {
    name   :: String
  , age    :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s  = Just s

noNegative :: Int -> Maybe Int
noNegative n
  | n > 0    = Just n
  | otherwise = Nothing

{-
cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' =
    case noEmpty name' of
      Nothing -> Nothing
      Just n  ->
        case noNegative age' of
          Nothing -> Nothing
          Just a  ->
            case noNegative weight' of
              Nothing -> Nothing
              Just w  ->
                Just (Cow n a w)
-}

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' =
  Cow <$> noEmpty name'
      <*> noNegative age'
      <*> noNegative weight'
------------------------------------------------------------------
data Validation err a =
    Failure err
  | Success a
  deriving (Eq, Show)

-- natural transformations
validToEither :: Validation e a -> Either e a
validEither (Failure err) = Left err
validToEither (Success a) = Right a

eitherToValid :: Either e a -> Validation e a
eitherToValid (Left err) = Failure err
eitherToValid (Right a)  = Success a

instance Functor (Validation e) where
  fmap _ (Failure err) = Failure err
  fmap g (Success a)   = Success $ g a

instance Monoid e => Applicative (Validation e) where
  pure  = Success
  (<*>) (Failure e) (Success a) = Failure e
  (<*>) (Failure e) (Failure x) = Failure (e <> x)
  (<*>) (Success g) (Success a) = Success $ g a

data Errors =
    DevideByZero
  | StackOverflow
  | MooglesChewedWires
  deriving (Eq, Show)
