module Chapter9 where

import Data.Bool (bool)
import Data.Char

myEnumFromTo :: (Ord a, Enum a, Bounded a, Eq a) => a -> a -> [a]
myEnumFromTo x y = reverse $ go x []
  where go x' acc
          | y < x' = []
          | x' == maxBound || x' == y = x':acc
          | otherwise = go (succ x') (x':acc)

eftBool :: Bool -> Bool -> [Bool]
eftBool = myEnumFromTo

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = myEnumFromTo

eftInt :: Int -> Int -> [Int]
eftInt = myEnumFromTo

eftChar :: Char -> Char -> String
eftChar = myEnumFromTo

myWords :: String -> [String]
myWords = splitBy ' '

splitBy :: Char -> String -> [String]
splitBy _ [] = []
splitBy c s = takeWhile (/= c) s : rest
  where rest = splitBy c (dropWhile (== c) (dropWhile (/= c) s))

mapWithBool :: (Num a, Eq a, Enum a) => [a]
mapWithBool = map (\x -> bool x (-x) (x == 3)) [1..10]

multiplesOfThree :: (Integral a) => [a]
multiplesOfThree = filter (\x -> x `mod` 3 == 0) [1..30]

multiplesOfThreeLength :: (Eq a, Integral a) => [a] -> Int
multiplesOfThreeLength = length . filter (\x -> x `mod` 3 == 0)

myFilter :: String -> [String]
myFilter = filter (/= "a") . filter (/= "the") . words

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myZip_ :: [a] -> [b] -> [(a, b)]
myZip_ = myZipWith (,)

allUpper :: String -> String
allUpper = filter isUpper

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

allToUpper :: String -> String
allToUpper [] = []
allToUpper (x:xs) = toUpper x : allToUpper xs

capitalizeFirst :: String -> Char
capitalizeFirst = toUpper . head

myOr ::[Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . fmap f

myElem :: Eq a => a -> [a] -> Bool
myElem = myAny . (==)

myReverse :: [a] -> [a]
myReverse x = go x []
  where
    go [] acc = acc
    go (y:ys) acc = go ys (y:acc)

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy _ [x] = x
myMaximumBy f (x:xs) = go xs x
  where go [] m = m
        go (y:ys) m = go ys newM
          where newM = if f y m == GT then y else m

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = undefined
myMinimumBy _ [x] = x
myMinimumBy f (x:xs) = go xs x
  where go [] m = m
        go (y:ys) m = go ys newM
          where newM = if f y m == LT then y else m

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
