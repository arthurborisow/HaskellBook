module Chapter12 where

import Data.List

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x = Just x

replaceThe :: String -> String
replaceThe = unwords . fmap (f . notThe) . words
  where f :: Maybe String -> String
        f Nothing = "a"
        f (Just x) = x

vowels :: String
vowels = "aeiou"

isVowel :: Char -> Bool
isVowel = (`elem` vowels)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go 0 . words
  where go n [] = n
        go n ["the"] = n
        go n ("the":xs) = if isVowel (head . head $ xs)
                          then go (n + 1) xs
                          else go n xs
        go n (_:xs) = go n xs

countVowels :: String -> Integer
countVowels = fromIntegral . length . filter isVowel

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s = let (v, c) = partition isVowel s
           in if length v > length c then Nothing else Just (Word' s)

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ s) = 1 + natToInteger s

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | otherwise = Just (go i Zero)
    where go 0 nat = nat
          go n nat = go (n - 1) (Succ nat)

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just a) = a
fromMaybe a _ = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList _ = []

catMaybes :: [Maybe a] -> [a]
catMaybes = fmap (fromMaybe undefined) . filter isJust

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr f (Just [])
  where f (Just x) (Just xs) = Just (x:xs)
        f _ _ = Nothing

lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where f (Left a) xs = a:xs
        f _ xs = xs

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where f (Right b) xs = b:xs
        f _ xs = xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr f ([], [])
  where f (Left a) (x, y)  = (a:x, y)
        f (Right b) (x, y) = (x, b:y)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right a) = Just $ f a

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ f (Right b) = f b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left _) = Nothing
eitherMaybe'' f (Right b) = Just $ f b

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
                  Nothing -> []
                  Just (x, y) -> x : myUnfoldr f y

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = case f x of
               Nothing -> Leaf
               Just (a, b, a') -> Node (unfold f a) b (unfold f a')

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
  where f x | x >= n = Nothing
            | otherwise = Just (x + 1, x, x + 1)
