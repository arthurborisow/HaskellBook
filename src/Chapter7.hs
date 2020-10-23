module Chapter7 where

addOneIfOdd :: Integral a => a -> a
addOneIfOdd =
  \n ->
    case odd n of
      True -> f n
      False -> n
      where f x = x + 1

addFive :: (Num a, Ord a) => a -> a -> a
addFive = \x -> \y -> (if x > y then y else x) + 5

mflip :: (a -> b -> c) -> b -> a -> c
mflip f' = \x -> \y -> f' y x

k :: (a, b) -> a
k (x, y) = x

k1 :: Num a => a
k1 = k ((4-1), 10)

k2 :: String
k2 = k ("three", (1 + 2))

k3 :: Num a => a -- will return three
k3 = k (3, True)

f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))

functionC :: Ord a => a -> a -> a
functionC x y = case (x > y) of
                  True -> x
                  False -> y

ifEvenAdd2 :: Integral a => a -> a
ifEvenAdd2 n = case even n of
                 True -> (n+2)
                 False -> n

nums :: (Ord a, Num a) => a -> Int
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 ='A'
  | y >= 0.8 ='B'
  | y >= 0.7 ='C'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where y = x / 100

pal :: Eq a => [a] -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise = False

numbers :: (Num a, Ord a) => a -> Int
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1

tensDigit :: Integral a => a -> a
tensDigit x = d
  where (_, d) = x `divMod` 10

hunsD :: Integral a => a -> a
hunsD = snd . (`divMod` 100)

foldBool :: a -> a -> Bool -> a
foldBool x y b =
  case b of
    True -> x
    False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y b
  | b = x
  | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g fun (a, c) = (fun a, c)


roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

roundTrip' :: (Show a, Read b) => a -> b
roundTrip' = read . show
-- print ((roundTrip' (4 :: Int)) :: Int) -- to eliminate all the warnings
