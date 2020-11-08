module Chapter18 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad (join, liftM2, ap, forM)

data Sum a b = First a | Second b deriving (Eq, Show)
instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)
instance Applicative (Sum a) where
  pure = Second
  Second f <*> Second b = Second (f b)
  First a <*> _ = First a
  _ <*> First a = First a
instance Monad (Sum a) where
  return = pure
  First a >>= _ = First a
  Second b >>= f = f b
instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq
instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = oneof [First <$> arbitrary, Second <$> arbitrary]


data Nope a = NopeDotJpg deriving (Eq, Show)
instance Functor Nope where
  fmap _ _ = NopeDotJpg
instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg
instance Monad Nope where
  _ >>= _ = NopeDotJpg
instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg
instance EqProp (Nope a) where
  (=-=) = eq

data PhhhbbtttEither b a = Left' a | Right' b deriving  (Show, Eq)
instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a) = Left' (f a)
  fmap _ (Right' b) = Right' b
instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  Right' b <*> _ = Right' b
  _ <*> Right' b = Right' b
  Left' f <*> Left' a = Left' (f a)
instance Monad (PhhhbbtttEither b) where
  Right' b >>= _ = Right' b
  Left' a >>= f = f a
instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = oneof [Left' <$> arbitrary, Right' <$> arbitrary]
instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

newtype Identity a = Identity a deriving (Eq, Ord, Show)
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)
instance Monad Identity where
  Identity a >>= f = f a
instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary
instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

data List a = Nil | Cons a (List a) deriving (Show, Eq)
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a r) = Cons (f a) (fmap f r)
instance Applicative List where
  pure = flip Cons Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f r <*> a = fmap f a `append` (r <*> a)
instance Monad List where
  Nil >>= _ = Nil
  Cons a r >>= f = f a `append` (r >>= f)
instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = oneof [return Nil, Cons <$> arbitrary <*> arbitrary]
instance (Eq a) => EqProp (List a) where
  (=-=) = eq

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

j :: Monad m => m (m a) -> m a
j m = m >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f m1 m2 = f <$> m1 <*> m2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh l f = foldr (\x y -> f x >>= \x' -> fmap (x' :) y) (pure []) l

flipType :: (Monad m) => [m a] -> m [a]
flipType i = meh i id

main :: IO ()
main = do
  quickBatch (functor (undefined :: Sum String (String, Double, Int)))
  quickBatch (applicative (undefined :: Sum String (String, Double, Int)))
  quickBatch (monad (undefined :: Sum String (String, Double, Int)))

  quickBatch (functor (undefined :: Nope (String, Double, Int)))
  quickBatch (applicative (undefined :: Nope (String, Double, Int)))
  quickBatch (monad (undefined :: Nope (String, Double, Int)))

  quickBatch (functor (undefined :: PhhhbbtttEither String (String, Double, Int)))
  quickBatch (applicative (undefined :: PhhhbbtttEither String (String, Double, Int)))
  quickBatch (monad (undefined :: PhhhbbtttEither String (String, Double, Int)))

  quickBatch (functor (undefined :: Identity (String, Double, Int)))
  quickBatch (applicative (undefined :: Identity (String, Double, Int)))
  quickBatch (monad (undefined :: Identity (String, Double, Int)))

  quickBatch (functor (undefined :: List (String, Double, Int)))
  quickBatch (applicative (undefined :: List (String, Double, Int)))
  quickBatch (monad (undefined :: List (String, Double, Int)))
