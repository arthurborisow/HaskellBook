{-# LANGUAGE InstanceSigs #-}

module Chapter22 where

import Control.Applicative
import Data.Maybe
import Data.Char

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> composed <*> fmapped

tMon :: [Char] -> ([Char], [Char])
tMon = do
  l <- composed
  r <- fmapped

  return (l, r)

newtype Reader r a = Reader {runReader :: r -> a}

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ \x -> f (ra x)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  --  pure a = Reader $ \_ -> a
  pure = Reader . const

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  --  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)
  Reader fa <*> Reader ra = Reader $ fa <*> ra

instance Monad (Reader r) where
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r

newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName,
    dogName :: DogName,
    address :: Address
  }
  deriving (Eq, Show)

data Dog = Dog
  { dogsName :: DogName,
    dogsAddress :: Address
  }
  deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

getDogRM'' :: Reader Person Dog
getDogRM'' = do
  name <- asks dogName
  addy <- asks address
  return $ Dog name addy

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks = Reader

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb


x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- it's also nice to have one that
-- will return Nothing, like this one
-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- now zip x and z using a
-- variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool -- use &&, >3, <8
bolt = (&&) <$> (>3) <*> (<8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
--  print $ sequenceA [Just 3, Just 2, Just 1]
--  print $ sequenceA [x, y]
--  print $ sequenceA [xs, ys]
--  print $ summed <$> ((,) <$> xs <*> ys)
--  print $ fmap summed ((,) <$> xs <*> zs)
--  print $ bolt 7
--  print $ fmap bolt z
--  print $ sequenceA [(>3), (<8), even] 7
  print $ foldr (&&) False $ sequA 15
  print $ sequA (fromMaybe 0 s')
  print $ bolt (fromMaybe 0 ys)
