module Chapter8 where

import Data.List (intercalate)

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

sum' :: (Eq a, Num a) => a -> a
sum' 1 = 1
sum' n = n + sum' (n - 1)

mul :: (Integral a) => a -> a -> a
mul x 1 = x
mul x y = x + mul x (y - 1)

data DividedResult a
  = Result a
  | DividedByZero deriving Show

dividedBy :: Integral a => a -> a -> DividedResult a
dividedBy _ 0 = DividedByZero
dividedBy x y = Result $ signum x * signum y * go (abs x) 0
  where go x' n
          | x' < d = n
          | otherwise = go (x' - d) (n + 1)
        d = abs y

mc91 :: Integral a => a -> a
mc91 x
  | x > 100 = x - 10
  | otherwise = mc91 . mc91 $ x + 11

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = undefined

digits :: Int -> [Int]
digits n = go n []
  where go :: Int -> [Int] -> [Int]
        go 0 c = c
        go n' c = go n'' (m:c)
          where
            (n'', m) = n' `divMod` 10

wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits

