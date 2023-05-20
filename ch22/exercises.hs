import           Data.Char
import           Examples
{-# LANGUAGE InstanceSigs #-}

------------------------------------------------------------------
--Exercise
------------------------------------------------------------------

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = (rev . cap)

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

--tupled :: [Char] -> ([Char], [Char])
--tupled = (,) <$> rev <*> cap

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
  r <- rev
  c <- cap
  return ((r,c))

tupledMB :: [Char] -> ([Char], [Char])
tupledMB = rev >>= \r -> cap >>= \c -> return ((r, c))

newtype Reader r a =
  Reader { runReader :: r -> a}

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ (f . ra)

ask :: Reader a a
ask = Reader id

------------------------------------------------------------------
--Exercise
------------------------------------------------------------------

--myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
--myLiftA2 f x y = f <$> x <*> y

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) (Reader rg) (Reader ra) = Reader $ \r -> rg r $ ra r

getDogRG :: Reader Person Dog
getDogRG = Dog <$> Reader dogName <*> Reader address
