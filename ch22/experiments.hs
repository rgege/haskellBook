newtype Voltage = Voltage String deriving (Eq, Show)
newtype Current = Current String deriving (Eq, Show)

data SourceMeter =
   SourceMeter {
      voltage :: Voltage
    , current :: Current
  } deriving (Eq, Show)

data Measurement =
  Measurement {
    voltageapp :: Voltage
  , currentapp :: Current
  } deriving (Eq, Show)

setParam :: Voltage -> Current -> SourceMeter
setParam = SourceMeter

newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader g) = Reader $ (f . g)

instance Applicative (Reader r) where
  pure a = Reader $ const a
  (<*>) (Reader f) (Reader g) = Reader $ \r -> (f r) (g r)


applyParam :: Reader SourceMeter Measurement
applyParam = Measurement <$> Reader voltage <*> Reader current

mkMeasurement :: SourceMeter -> Measurement
mkMeasurement = runReader applyParam


