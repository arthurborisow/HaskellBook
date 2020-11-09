module Chapter21 where

import Control.Monad ((<=<))
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Query     = Query
data SomeObj   = SomeObj
data IoOnlyObj = IoOnlyObj
data Err       = Err
-- There's a decoder function that makes -- some object from String
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined
-- There's a query, that runs against the -- DB and returns array of strings
fetchFn :: Query -> IO [String]
fetchFn = undefined
-- an additional "context initializer", -- that also has IO
makeIoOnlyObj :: [SomeObj]-> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn = (traverse makeIoOnlyObj . traverse decodeFn) <=< fetchFn
--  case traverse decodeFn a of
--    (Left err) -> return $ Left $ err
--    (Right res) -> do
--      a <- makeIoOnlyObj res
--      return $ Right a

newtype Identity a = Identity a deriving (Eq, Ord, Show)
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
instance Foldable Identity where
  foldr f z (Identity a) = f a z
instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a
instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary
instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)
instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a
instance Foldable (Constant a) where
  foldMap _ _ = mempty
instance Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a
instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary
instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

data Optional a = Nada | Yep a deriving (Show, Eq)
instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)
instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep a) = f a z
instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a
instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = oneof [return Nada, Yep <$> arbitrary]
instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq

data List a = Nil | Cons a (List a) deriving (Show, Eq)
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
instance Foldable List where
  foldr _ z Nil = z
  foldr f z (Cons x xs) = f x (foldr f z xs)
instance Traversable List where
  traverse f l = foldr cons (pure Nil) l
    where cons x y = Cons <$> f x <*> y
instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = oneof [return Nil, Cons <$> arbitrary <*> arbitrary]
instance (Eq a) => EqProp (List a) where
  (=-=) = eq

data Three a b c = Three a b c deriving (Show, Eq)
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)
instance Foldable (Three a b) where
  foldr f z (Three _ _ c) = f c z
instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary
instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

data Pair a b = Pair a b deriving (Show, Eq)
instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)
instance Foldable (Pair a) where
  foldr f z (Pair _ b) = f b z
instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b
instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary
instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

data Big a b = Big a b b deriving (Show, Eq)
instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')
instance Foldable (Big a) where
  foldMap f (Big _ b b') = f b <> f b'
instance Traversable (Big a) where
  traverse f (Big a b b') = Big a <$> f b <*> f b'
instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary
instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

data Bigger a b = Bigger a b b b deriving (Show, Eq)
instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')
instance Foldable (Bigger a) where
  foldMap f (Bigger _ b b' b'') = f b <> f b' <> f b''
instance Traversable (Bigger a) where
  traverse f (Bigger a b b' b'') = Bigger a <$> f b <*> f b' <*> f b''
instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

data S n a = S (n a) a deriving (Eq, Show)
instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary
instance (Applicative n, Testable (n Property), Eq a, Eq (n a), EqProp a) => EqProp (S n a) where
  (=-=) = eq
instance (Functor n) => Functor (S n) where
  fmap f (S n a) = S (fmap f n) (f a)
instance (Foldable n) => Foldable (S n) where
  foldMap f (S n a) = foldMap f n <> f a
instance Traversable n => Traversable (S n) where
  traverse f (S n a) = S <$> traverse f n <*> f a

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)
instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node fa a fb) = Node (f <$> fa) (f a) (f <$> fb)
-- foldMap is a bit easier -- and looks more natural, -- but you can do foldr too -- for extra credit.
instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node fa a fb) = foldMap f fa <> f a <> foldMap f fb
instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node fa a fb) = Node <$> traverse f fa <*> f a <*> traverse f fb
instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = oneof [return Empty, Leaf <$> arbitrary, Node <$> arbitrary <*> arbitrary <*> arbitrary]
instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch (traversable (undefined :: Identity (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Constant String (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Optional (Int, Int, [Int])))
  quickBatch (traversable (undefined :: List (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Three String Double (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Pair String (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Big String (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Bigger String (Int, Int, [Int])))
  quickBatch (traversable (undefined :: S [] (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Tree (Int, Int, [Int])))
