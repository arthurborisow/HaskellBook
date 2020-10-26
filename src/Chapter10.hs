module Chapter10 where

import Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
    DbNumber 9001,
    DbString "Hello, world!",
    DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

isDbDate :: DatabaseItem -> Bool
isDbDate (DbDate _) = True
isDbDate _ = False

dateFromDbDate :: DatabaseItem -> UTCTime
dateFromDbDate (DbDate t) = t
dateFromDbDate _ = undefined

isDbNumber :: DatabaseItem -> Bool
isDbNumber (DbNumber _) = True
isDbNumber _ = False

numberFromDbNumber :: DatabaseItem -> Integer
numberFromDbNumber (DbNumber n) = n
numberFromDbNumber _ = undefined

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = map dateFromDbDate . filter isDbDate

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = map numberFromDbNumber . filter isDbNumber

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = minimum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = (fromIntegral . sum $ numbers) / (fromIntegral . length $ numbers)
  where
    numbers = filterDbNumber db

fibs :: [Integer]
fibs = takeWhile (< 100) . take 20 $ 1 : scanl (+) 1 fibs

fibsN :: Int -> Integer
fibsN x = fibs !! x

fact :: [Integer]
fact = scanl (*) 1 [2 ..]

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

stopVowelsStops :: [(Char, Char, Char)]
stopVowelsStops =
  [ (x, y, z) | x <- stops, x == 'p', y <- vowels, z <- stops
  ]

-- former seekritFunc
averageWordLength :: String -> Double
averageWordLength x = sumOfLengths / size
  where
    wrds = words x
    size = fromIntegral . length $ wrds
    sumOfLengths = fromIntegral . sum . map length $ wrds

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x y -> f x || y) False

myElemFold :: Eq a => a -> [a] -> Bool
myElemFold x = foldr (\y z -> x == y || z) False

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny = myAny . (==)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> f x : y) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x y -> if f x then x : y else y) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . fmap f

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy _ [x] = x
myMaximumBy f l@(x:_) = foldl (\y z -> if f y z == GT then y else z) x l

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = undefined
myMinimumBy _ [x] = x
myMinimumBy f l@(x:_) = foldl (\y z -> if f y z == LT then y else z) x l
