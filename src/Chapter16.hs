{-# LANGUAGE FlexibleInstances #-}

module Chapter16 where

import Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)


newtype Identity a = Identity a deriving (Show, Eq)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)


data Pair a = Pair a a deriving (Show, Eq)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)


data Two a b = Two a b deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)


data Three a b c = Three a b c deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)


data Three' a b = Three' a b b deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)


data Four a b c d = Four a b c d deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)


data Four' a b = Four' a a a b deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary = oneof [return LolNope, Yeppers <$> arbitrary]

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)


data Sum a b = First a | Second b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = oneof [First <$> arbitrary, Second <$> arbitrary]

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second a) = Second (f a)

data Quant a b = Finance | Desk a | Bloor b deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = oneof [return Finance, Desk <$> arbitrary, Bloor <$> arbitrary]

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

newtype K a b = Ka a deriving (Show, Eq)

instance (Arbitrary a) => Arbitrary (K a b) where
  arbitrary = Ka <$> arbitrary

instance Functor (K a) where
  fmap _ (Ka a) = Ka a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (Ka a)) = Flip (Ka (f a))

newtype EvilGoateeConst a b = GoatyConst b deriving (Show, Eq)

instance (Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
  arbitrary = GoatyConst <$> arbitrary

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst a) = GoatyConst (f a)

newtype LiftItOut f a = LiftItOut (f a) deriving (Show, Eq)

instance (Arbitrary (f a)) => Arbitrary (LiftItOut f a) where
  arbitrary = LiftItOut <$> arbitrary

instance (Functor f) => Functor (LiftItOut f) where
  fmap f (LiftItOut f') = LiftItOut (f <$> f')


data Parappa f g a = DaWrappa (f a) (g a) deriving (Show, Eq)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f' (DaWrappa f g) = DaWrappa (fmap f' f) (fmap f' g)

instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary (Parappa f g a) where
  arbitrary = DaWrappa <$> arbitrary <*> arbitrary


data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Show, Eq)

instance (Arbitrary (f a), Arbitrary (g b)) => Arbitrary (IgnoreOne f g a b) where
  arbitrary = IgnoringSomething <$> arbitrary <*> arbitrary

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f' (IgnoringSomething f g) = IgnoringSomething f (fmap f' g)


data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Show, Eq)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious x y z) = Notorious x y (fmap f z)

instance (Arbitrary (g o), Arbitrary (g a), Arbitrary (g t)) => Arbitrary (Notorious g o a t) where
  arbitrary = Notorious <$> arbitrary <*> arbitrary <*> arbitrary


data List a = Nil | Cons a (List a) deriving (Show, Eq)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = oneof [return Nil, Cons <$> arbitrary <*> arbitrary]


data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving (Show, Eq)

instance (Arbitrary a) => Arbitrary (GoatLord a) where
  arbitrary = oneof [return NoGoat, OneGoat <$> arbitrary, MoreGoats <$> arbitrary <*> arbitrary <*> arbitrary]

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)


data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print x a) = Print x (f a)
  fmap f (Read f') = Read (f . f')

runSpecs :: IO ()
runSpecs = do
  quickCheck (functorIdentity :: Identity Int -> Bool)
  quickCheck (\x -> functorCompose (+1) (*2) (x :: Identity Int))

  quickCheck (functorIdentity :: Pair Int -> Bool)
  quickCheck (\x -> functorCompose (+1) (*2) (x :: Pair Int))

  quickCheck (functorIdentity :: Two String Int -> Bool)
  quickCheck (\x -> functorCompose (+1) (*2) (x :: Two String Int))

  quickCheck (functorIdentity :: Three [Int] String Int -> Bool)
  quickCheck (\x -> functorCompose (+1) (*2) (x :: Three [Int] String Int))

  quickCheck (functorIdentity :: Three' String Int -> Bool)
  quickCheck (\x -> functorCompose (+1) (*2) (x :: Three' String Int))

  quickCheck (functorIdentity :: Four [String] Bool String Int -> Bool)
  quickCheck (\x -> functorCompose (+1) (*2) (x :: Four [String] Bool String Int))

  quickCheck (functorIdentity :: Four' String Int -> Bool)
  quickCheck (\x -> functorCompose (+1) (*2) (x :: Four' String Int))

  quickCheck (functorIdentity :: Possibly Int -> Bool)
  quickCheck (\x -> functorCompose (+1) (*2) (x :: Possibly Int))

  quickCheck (functorIdentity :: Sum String Int -> Bool)
  quickCheck (\x -> functorCompose (+1) (*2) (x :: Sum String Int))

  quickCheck (functorIdentity :: Quant String Int -> Bool)
  quickCheck (\x -> functorCompose (+1) (*2) (x :: Quant String Int))

  quickCheck (functorIdentity :: K String Int -> Bool)
  quickCheck (\x -> functorCompose (+1) (*2) (x :: K String Int))

  quickCheck (functorIdentity :: EvilGoateeConst String Int -> Bool)
  quickCheck (\x -> functorCompose (+1) (*2) (x :: EvilGoateeConst String Int))

  quickCheck (functorIdentity :: LiftItOut [] Int -> Bool)
  quickCheck (\x -> functorCompose (+1) (*2) (x :: LiftItOut [] Int))

  quickCheck (functorIdentity :: Parappa Maybe [] Int -> Bool)
  quickCheck (\x -> functorCompose (+1) (*2) (x :: Parappa Maybe [] Int))

  quickCheck (functorIdentity :: IgnoreOne Maybe [] String Int -> Bool)
  quickCheck (\x -> functorCompose (+1) (*2) (x :: IgnoreOne Maybe [] String Int))

  quickCheck (functorIdentity :: Notorious [] String String Int -> Bool)
  quickCheck (\x -> functorCompose (+1) (*2) (x :: Notorious [] String String Int))

  quickCheck (functorIdentity :: List Int -> Bool)
  quickCheck (\x -> functorCompose (+1) (*2) (x :: List Int))

  quickCheck (functorIdentity :: GoatLord Int -> Bool)
  quickCheck (\x -> functorCompose (+1) (*2) (x :: GoatLord Int))
