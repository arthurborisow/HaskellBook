{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Chapter26 where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class


newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  MaybeT fab <*> MaybeT mma = MaybeT $ fmap (<*>) fab <*> mma

instance (Monad m) => Monad (MaybeT m) where
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  MaybeT ma >>= f = MaybeT $ do
    v <- ma
    case v of
      Nothing -> return Nothing
      Just a -> runMaybeT $ f a

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT me) = EitherT $ (fmap . fmap) f me

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  EitherT mf <*> EitherT ma = EitherT $ fmap (<*>) mf <*> ma

instance Monad m => Monad (EitherT e m) where
  EitherT ma >>= f = EitherT $ do
    v <- ma
    case v of
      Left a -> return (Left a)
      Right a -> runEitherT $ f a

swapEither :: Either e a -> Either a e
swapEither (Left a) = Right a
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT e) = EitherT $ fmap swapEither e

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f f' (EitherT e) = do
  v <- e
  case v of
    Left a -> f a
    Right a -> f' a

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure
  ReaderT mf <*> ReaderT ma = ReaderT $ fmap (<*>) mf <*> ma

instance (Monad m) => Monad (ReaderT r m) where
  ReaderT ma >>= f = ReaderT $ \x -> do --ma x >>= (\y -> runReaderT (f y) x)
    y <- ma x

    runReaderT (f y) x

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT ms) = StateT $ (fmap . fmap) (\(a, s) -> (f a, s)) ms

instance (Monad m) => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)
  StateT mfa <*> StateT ma = StateT $ \s -> do
    (f, s') <- mfa s
    (a', s'') <- ma s'

    return (f a', s'')

instance (Monad m) => Monad (StateT s m) where
  StateT sma >>= f = StateT $ \s -> do
    (a, s') <- sma s

    runStateT (f a) s'

instance MonadTrans (EitherT e) where
  lift = EitherT . fmap pure

instance MonadTrans (StateT s) where
  lift ms = StateT $ \s -> (, s) <$> ms

instance MonadTrans MaybeT where
  lift = MaybeT . fmap pure

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO
