{-# LANGUAGE InstanceSigs #-}

module Chapter25 where

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)
instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose a) = Compose $ (fmap . fmap) f a

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  Compose f <*> Compose a = Compose $ fmap (<*>) f <*> a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose x) = (foldMap . foldMap) f x

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose x) = Compose <$> (traverse . traverse) f x


class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b = Deux a b
instance Bifunctor Deux where
  bimap ab cd (Deux a c) = Deux (ab a) (cd c)

newtype Const a b = Const a
instance Bifunctor Const where
  bimap ab _ (Const a) = Const (ab a)

data Drei a b c = Drei a b c
instance Bifunctor (Drei a) where
  bimap ab cd (Drei k a c) = Drei k (ab a) (cd c)

data SuperDrei a b c = SuperDrei a b
instance Bifunctor (SuperDrei a) where
  bimap ab _ (SuperDrei a c) = SuperDrei a (ab c)


newtype SemiDrei a b c = SemiDrei a
instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d
instance Bifunctor (Quadriceps a b) where
  bimap ab cd (Quadzzz k l a c) = Quadzzz k l (ab a) (cd c)

instance Bifunctor Either where
  bimap f _ (Left a) = Left (f a)
  bimap _ f (Right a) = Right (f a)

newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance (Applicative m) => Applicative (IdentityT m) where
  pure = IdentityT . pure
  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

instance (Monad m) => Monad (IdentityT m) where
  i >>= f = IdentityT $ runIdentityT . f =<< runIdentityT i
