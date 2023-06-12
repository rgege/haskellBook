import           Data.Time

data DatabaseItem =
    DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 21
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

-- foldr :: (a -> b -> b) -> b -> [a] -> b

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr (\a b -> case a of
                                (DbDate time) -> time : b
                                otherwise     -> b ) []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr (\a b -> case a of
                                 (DbNumber num) -> num : b
                                 otherwise      -> b) []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr max zeroVal . filterDbDate
  where zeroVal = UTCTime (fromGregorian 0000 00 00) (secondsToDiffTime 0)

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = (fromIntegral . sumDb) db / (fromIntegral . length . filterDbNumber) db
