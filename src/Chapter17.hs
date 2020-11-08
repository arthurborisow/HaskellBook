module Chapter17 where

import Control.Applicative (liftA3)
import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant m <*> Constant m' = Constant (m <> m')


data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = oneof [return Nil, Cons <$> arbitrary <*> arbitrary]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

instance Semigroup (List a) where
  (<>) = append

instance (Monoid a) => Monoid (List a) where
  mempty = pure mempty

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a r) = Cons (f a) (fmap f r)

instance Applicative List where
  pure = flip Cons Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f r <*> a = fmap f a `append` (r <*> a)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- write this one in terms
-- of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as


take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _ = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs in take' 3000 l
          ys' = let (ZipList' l) = ys in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure = ZipList' . repeat'
  ZipList' Nil <*> _ = ZipList' Nil
  _ <*> ZipList' Nil = ZipList' Nil
  ZipList' fs <*> ZipList' xs = ZipList' (zipListsWith id fs xs)

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

zipListsWith :: (a -> b -> c) -> List a -> List b -> List c
zipListsWith _ Nil _ =  Nil
zipListsWith _ _ Nil =  Nil
zipListsWith f (Cons x xs) (Cons y ys) = Cons (f x y) (zipListsWith f xs ys)

instance (Arbitrary a) => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

data Validation e a = Failure e | Success a deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Success
  Success _ <*> Failure e = Failure e
  Failure e <*> Success _ = Failure e
  Failure e <*> Failure e' = Failure (e <> e')
  Success f <*> Success a = Success (f a)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = oneof [Failure <$> arbitrary, Success <$> arbitrary]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure x = Pair x x
  Pair f f' <*> Pair x x' = Pair (f x) (f' x')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq


data Two a b = Two a b deriving (Show, Eq)
instance Functor (Two a) where fmap f (Two a b) = Two a (f b)
instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  Two a f <*> Two a' x = Two (a <> a') (f x)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where arbitrary = Two <$> arbitrary <*> arbitrary
instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

data Three a b c = Three a b c deriving (Show, Eq)
instance Functor (Three a b) where fmap f (Three a b c) = Three a b (f c)
instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  Three a b f <*> Three a' b' x = Three (a <> a') (b <> b') (f x)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary
instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

data Three' a b = Three' a b b deriving (Show, Eq)
instance Functor (Three' a) where fmap f (Three' a b b') = Three' a (f b) (f b')
instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  Three' a f f' <*> Three' a' b b' = Three' (a <> a') (f b) (f' b')
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary
instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

data Four a b c d = Four a b c d deriving (Show, Eq)
instance Functor (Four a b c) where fmap f (Four a b c d) = Four a b c (f d)
instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  Four a b c f <*> Four a' b' c' x = Four (a <> a') (b <> b') (c <> c') (f x)
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where (=-=) = eq

data Four' a b = Four' a a a b deriving (Show, Eq)
instance Functor (Four' a) where fmap f (Four' a a' a'' d) = Four' a a' a'' (f d)
instance (Monoid a) => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  Four' a b c f <*> Four' a' b' c' x = Four' (a <> a') (b <> b') (c <> c') (f x)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

main :: IO ()
main = do
  quickBatch (applicative (undefined :: List (String, Double, Int)))
  quickBatch (applicative (undefined :: ZipList' (String, Double, Int)))
  quickBatch (applicative (undefined :: Validation [String] (String, String, Int)))
  quickBatch (applicative (undefined :: Pair (String, Double, Int)))
  quickBatch (applicative (undefined :: Two String (String, Double, Int)))
  quickBatch (applicative (undefined :: Three [Int] String (String, Double, Int)))
  quickBatch (applicative (undefined :: Three' String (String, Double, Int)))
  quickBatch (applicative (undefined :: Four [Int] String [Double] (String, Double, Int)))
  quickBatch (applicative (undefined :: Four' [String] (String, Double, Int)))
