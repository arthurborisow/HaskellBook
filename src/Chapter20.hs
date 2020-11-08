module Chapter20 where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = foldr ((||) . (== x)) False

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr f Nothing
  where
    f x Nothing = Just x
    f x (Just y) = Just $ if x < y then x else y

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr f Nothing
  where
    f x Nothing = Just x
    f x (Just y) = Just $ if x > y then x else y

null :: (Foldable t) => t a -> Bool
null = foldr (const . const False) True

length :: (Foldable t) => t a -> Int
length = foldr (const (+ 1)) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr ((<>) . f) mempty

newtype Constant a b = Constant b deriving (Show, Eq)
instance Foldable (Constant a) where
  foldMap f (Constant b) = f b
instance (Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

data Two a b = Two a b deriving (Show, Eq)
instance Foldable (Two a) where
  foldMap f (Two _ b) = f b
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

data Three a b c = Three a b c deriving (Show, Eq)
instance Foldable (Three a b) where
  foldr f z (Three _ _ c) = f c z
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

data Three' a b = Three' a b b deriving (Show, Eq)
instance Foldable (Three' a) where
  foldr f z (Three' _ b b') = f b (f b' z)
  foldMap f (Three' _ b b') = f b <> f b'
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

data Four' a b = Four' a b b b deriving (Show, Eq)
instance Foldable (Four' a) where
  foldMap f (Four' _ b b' b'') = f b <> f b' <> f b''
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

filterF :: (Applicative f , Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)

main :: IO ()
main = do
  quickBatch (foldable (undefined :: Constant Int (String, Sum Int, Product Double, Integer, String)))
  quickBatch (foldable (undefined :: Two Int (String, Sum Int, Product Double, Integer, String)))
  quickBatch (foldable (undefined :: Three Int String (String, Sum Int, Product Double, Integer, String)))
  quickBatch (foldable (undefined :: Three' Int (String, Sum Int, Product Double, Integer, String)))
  quickBatch (foldable (undefined :: Four' Int (String, Sum Int, Product Double, Integer, String)))
--foldable :: forall (t :: * -> *) a b m o n. (Foldable t, CoArbitrary a, CoArbitrary b, Arbitrary a, Arbitrary b, Arbitrary m, Arbitrary o, Arbitrary (t a), Arbitrary (t m), Arbitrary (t n), Arbitrary (t o), Monoid m, Num n, Ord o, EqProp m, EqProp n, EqProp b, EqProp o, EqProp a, Show b, Show o, Show (t m), Show (t n), Show (t o), Show (t a)) => t (a, b, m, n,
