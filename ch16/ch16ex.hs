import           GHC.Arr
{-# LANGUAGE FlexibleInstances #-}
-------------------------------------------------------------------------------
-- Maybe : Short Exercise
-------------------------------------------------------------------------------

-- data structure identical to Maybe

data Possibly a =
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope     = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

-------------------------------------------------------------------------------
-- Either : Short Exercise
-------------------------------------------------------------------------------

-- data structure identical to Either

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a)  = First a
  fmap f (Second b) = Second (f b)

-- Either is a  * -> * -> * kind not a valid functor
-- (Either a) is a * -> * valid functor

-------------------------------------------------------------------------------
-- Chapter Exercises
-------------------------------------------------------------------------------
--1. :k Bool :: * -> no valid Functor
data Bool = False | True
--
--2.
data BoolAndSomethingElse a=
  False' a | True' a

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a)  = True' (f a)
--3.
data BoolAndMaybeSomethingElse a =
  Falsish | Truish a

instance Functor BoolAndMaybeSomethingElse where
  fmap f Falsish    = Falsish
  fmap f (Truish a) = Truish (f a)
--4. wrong kind :k Mu :: (* -> *) -> * and :k Mu f :: * functor must be * -> *
newtype Mu f =
    InF { outF :: f (Mu f) }


--5. :k D :: * -> novalid Functor
data D =
  D (Array Word Word) Int Int
-------------------------------------------------------------------------------
--1.
data Sum' b a =
    First' a
  | Second' b

instance Functor (Sum' b) where
  fmap f (First' a)  = First' (f a)
  fmap f (Second' b) = Second' b
--2.
data Company a c b =
    DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b)  = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c
--3.
data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More b) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'
-------------------------------------------------------------------------------
--1.
data Quant a b =
    Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a
  fmap f (Bloor b) = Bloor (f b)
--2.
data K a b =
  K a
  deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = K a
--3.
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip (K (f a))
--4.
data EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)
--5.
data LiftItOut f a =
  LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)
--6.
data Parappa f g a =
  DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)
--7.
data IgnoreOne f g a b =
  IgnoreSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoreSomething fa gb) = IgnoreSomething fa (fmap f gb)
--8.
data Notoriuos g o a t =
  Notoriuos (g o) (g a) (g t)

instance Functor g => Functor (Notoriuos g o a) where
  fmap f (Notoriuos go ga gt) = Notoriuos go ga (fmap f gt)
--9.
data List a =
    Nil
  | Cons a (List a)

instance Functor (List) where
  fmap _ Nil         = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)
--10.
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat               = NoGoat
  fmap f (OneGoat a)          = OneGoat (f a)
  fmap f (MoreGoats xa ya za) = MoreGoats (fmap f xa)
                                          (fmap f ya)
                                          (fmap f za)
--11.
data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read sa)   = Read (fmap f sa)
