{-# LANGUAGE TupleSections #-}

module Chapter15 where

import Data.Monoid
import Test.QuickCheck hiding (Failure, Success)

data Optional a = Nada | Only a deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = frequency [(1, return Nada), (1, fmap Only arbitrary)]

instance (Semigroup a) => Semigroup (Optional a) where
  Only a <> Only b = Only $ a <> b
  Nada <> a = a
  a <> Nada = a

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

type Verb = String

type Adjective = String

type Adverb = String

type Noun = String

type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  e <> "! he said "
    <> adv
    <> " as he jumped into his car "
    <> noun
    <> " and drove off with his "
    <> adj
    <> " wife."

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj = mconcat elems
  where
    elems = [e, "! he said ", adv, " as he jumped into his car ", noun, " and drove off with his ", adj, " wife."]

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

newtype First' a = First' {getFirst' :: Optional a} deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = fmap First' arbitrary

instance Semigroup (First' a) where
  First' o@(Only _) <> _ = First' o
  First' Nada <> a = a

instance Monoid (First' a) where
  mempty = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
  First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Show, Eq)

instance (Semigroup a) => Semigroup (Identity a) where
  Identity a <> Identity b = Identity $ a <> b

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

data Two a b = Two a b deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a b <> Two a' b' = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

data Three a b c = Three a b c deriving (Show, Eq)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  Three a b c <> Three a' b' c' = Three (a <> a') (b <> b') (c <> c')

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

type ThreeAssoc = Three String String String -> Three String String String -> Three String String String -> Bool

data Four a b c d = Four a b c d deriving (Show, Eq)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  Four a b c d <> Four a' b' c' d' = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

type FourAssoc = Four String String String String -> Four String String String String -> Four String String String String -> Bool

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj False <> _ = BoolConj False
  _ <> a = a

instance Monoid BoolConj where
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

type ConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj True <> _ = BoolDisj True
  _ <> a = a

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

type DisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

data Or a b = Fst a | Snd b deriving (Show, Eq)

instance Semigroup (Or a b) where
  s@(Snd _) <> _ = s
  _ <> s = s

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = frequency [(1, Fst <$> arbitrary), (1, Snd <$> arbitrary)]

type OrAssoc = Or String String -> Or String String -> Or String String -> Bool

newtype Combine a b = Combine {unCombine :: a -> b}

instance (Semigroup b) => Semigroup (Combine a b) where
  Combine f <> Combine f' = Combine $ f <> f'

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine mempty

combineAssoc :: (Eq b, Semigroup b) => a -> Combine a b -> Combine a b -> Combine a b -> Bool
combineAssoc x (Combine f) (Combine g) (Combine h) = (f x <> (g x <> h x)) == ((f x <> g x) <> h x)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = fmap Combine arbitrary

instance Show (Combine a b) where
  show _ = "Combine"

combLeftIdentity :: (Eq b, Monoid b) => a -> Combine a b -> Bool
combLeftIdentity i c = (unCombine (c <> mempty)) i == (unCombine c) i

combRightIdentity :: (Eq b, Monoid b) => a -> Combine a b -> Bool
combRightIdentity i c = (unCombine (mempty <> c)) i == (unCombine c) i

type CombAssoc = Int -> Combine Int (Sum Int) -> Combine Int (Sum Int) -> Combine Int (Sum Int) -> Bool

newtype Comp a = Comp {unComp :: a -> a}

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = fmap Comp arbitrary

instance (Semigroup a) => Semigroup (Comp a) where
  Comp f <> Comp f' = Comp (f <> f')

instance (Monoid a) => Monoid (Comp a) where
  mempty = Comp mempty

instance Show (Comp a) where
  show _ = "Comp"

compAssoc :: (Eq a, Semigroup a) => a -> Comp a -> Comp a -> Comp a -> Bool
compAssoc x (Comp f) (Comp g) (Comp h) = (f x <> (g x <> h x)) == ((f x <> g x) <> h x)

compLeftIdentity :: (Eq a, Monoid a) => a -> Comp a -> Bool
compLeftIdentity i c = (unComp (c <> mempty)) i == (unComp c) i

compRightIdentity :: (Eq a, Monoid a) => a -> Comp a -> Bool
compRightIdentity i c = (unComp (mempty <> c)) i == (unComp c) i

type CompAssoc = String -> Comp String -> Comp String -> Comp String -> Bool

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = oneof [Chapter15.Failure <$> arbitrary, Chapter15.Success <$> arbitrary]

instance (Semigroup a) => Semigroup (Validation a b) where
  Success x <> _ = Success x
  _ <> Success y = Success y
  Failure x <> Failure y = Failure (x <> y)

type ValidationAssoc = Validation String String -> Validation String String -> Validation String String -> Bool

newtype Mem s a = Mem {runMem :: s -> (a, s)}

instance Semigroup a => Semigroup (Mem s a) where
  Mem f <> Mem f' = Mem $ \s ->
    let (a', s') = f s
        (a'', s'') = f' s'
     in (a' <> a'', s'')

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (mempty,)

instance Show (Mem s a) where
  show _ = "Mem s a"

instance (Arbitrary a, CoArbitrary s, Arbitrary s) => Arbitrary (Mem s a) where
  arbitrary = Mem <$> arbitrary

type MemAssoc = Int -> Mem Int String -> Mem Int String -> Mem Int String -> Bool

memAssoc :: (Eq a, Eq b, Semigroup b) => a -> Mem a b -> Mem a b -> Mem a b -> Bool
memAssoc x f g h = runMem (f <> (g <> h)) x == runMem ((f <> g) <> h) x

memLeftIdentity :: (Eq a, Eq b, Monoid b) => a -> Mem a b -> Bool
memLeftIdentity i c = (runMem (c <> mempty)) i == (runMem c) i

memRightIdentity :: (Eq a, Eq b, Monoid b) => a -> Mem a b -> Bool
memRightIdentity i c = (runMem (mempty <> c)) i == (runMem c) i

--f' :: Mem Int String
--f' = Mem $ \s -> ("hi", s + 1)
--
--main :: IO ()
--main = do
--  let rmzero = runMem mempty 0
--      rmleft = runMem (f' <> mempty) 0
--      rmright = runMem (mempty <> f') 0
--  print $ rmleft
--  print $ rmright
--  print $ (rmzero :: (String, Int))
--  print $ rmleft == runMem f' 0
--  print $ rmright == runMem f' 0

runSpecs :: IO ()
runSpecs = do
  quickCheck (monoidAssoc :: Optional String -> Optional String -> Optional String -> Bool)
  quickCheck (monoidLeftIdentity :: Optional String -> Bool)
  quickCheck (monoidRightIdentity :: Optional String -> Bool)

  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)

  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)

  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (monoidLeftIdentity :: Three String String String -> Bool)
  quickCheck (monoidRightIdentity :: Three String String String -> Bool)

  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (monoidLeftIdentity :: Four String String String String -> Bool)
  quickCheck (monoidRightIdentity :: Four String String String String -> Bool)

  quickCheck (semigroupAssoc :: ConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)

  quickCheck (semigroupAssoc :: DisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

  quickCheck (semigroupAssoc :: OrAssoc)

  quickCheck (combineAssoc :: CombAssoc)
  quickCheck (combLeftIdentity :: Int -> Combine Int (Sum Int) -> Bool)
  quickCheck (combRightIdentity :: Int -> Combine Int (Sum Int) -> Bool)

  quickCheck (compAssoc :: CompAssoc)
  quickCheck (compLeftIdentity :: String -> Comp String -> Bool)
  quickCheck (compRightIdentity :: String -> Comp String -> Bool)

  quickCheck (semigroupAssoc :: ValidationAssoc)

  quickCheck (memAssoc :: MemAssoc)
  quickCheck (memLeftIdentity :: Int -> Mem Int String -> Bool)
  quickCheck (memRightIdentity :: Int -> Mem Int String -> Bool)
