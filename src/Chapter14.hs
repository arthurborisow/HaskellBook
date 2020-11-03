module Chapter14 where

import Test.QuickCheck
import Data.List (sort)
import Chapter11 (capitalizeWord)

half x = x / 2

halfIdentity = (*2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)

genList :: (Arbitrary a) => Gen [a]
genList = sized $
              \n -> do
                k <- choose (0, n)
                sequence [ arbitrary | _ <- [1..k] ]

prop_halvesNumber :: Double -> Bool
prop_halvesNumber x = half x == (half $ halfIdentity x)

prop_sorted :: Property
prop_sorted = forAll (genList :: Gen [Int]) (listOrdered . sort)

prop_plusAssociative :: Integer -> Integer -> Integer -> Bool
prop_plusAssociative x y z = x + (y + z) == (x + y) + z

prop_plusCommutative :: Integer -> Integer -> Bool
prop_plusCommutative x y = x + y == y + x

prop_mulAssociative :: Integer -> Integer -> Integer -> Bool
prop_mulAssociative x y z = x * (y * z) == (x * y) * z

prop_mulCommutative :: Integer -> Integer -> Bool
prop_mulCommutative x y = x * y == y * x

prop_expoAssociative :: Integer -> Integer -> Integer -> Bool
prop_expoAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

prop_expoCommutative :: Integer -> Integer -> Bool
prop_expoCommutative x y = x ^ y == y ^ x

prop_quotRem :: (Integral a) => NonZero a -> NonZero a -> Bool
prop_quotRem (NonZero x) (NonZero y) = (quot x y) * y + rem x y == x

prop_divMod :: (Integral a) => NonZero a -> NonZero a -> Bool
prop_divMod (NonZero x) (NonZero y) = (div x y) * y + mod x y == x

prop_reverse :: (Eq a) => [a] -> Bool
prop_reverse xs = (reverse . reverse $ xs) == xs

prop_dollar :: Integer -> Bool
prop_dollar x = (+ 1) x == ((+ 1) $ x)

prop_comp :: Integer -> Bool
prop_comp x = (*2) ((+ 1) x) == ((*2) . (+1)) x

prop_addLists :: (Eq a) => [a] -> [a] -> Bool
prop_addLists xs ys = foldr (:) xs ys == xs ++ ys

prop_concat :: (Eq a) => [a] -> [a] -> Bool
prop_concat xs ys = foldr (++) [] [xs, ys] == concat [xs, ys]

prop_length :: Positive Int -> NonEmptyList [Int] -> Bool
prop_length (Positive n) (NonEmpty xs) = length (take n xs) == n

prop_readShow :: (Show a, Read a, Eq a) => a -> Bool
prop_readShow x = (read (show x)) == x

twice f = f . f
fourTimes = twice . twice

prop_capitalize :: String -> Bool
prop_capitalize x = (capitalizeWord x
                    == twice capitalizeWord x)
                    &&
                    (capitalizeWord x
                    == fourTimes capitalizeWord x)

prop_sorted2 :: Property
prop_sorted2 = forAll (genList :: Gen [Int]) (\x -> (sort x
                                                    == twice sort x)
                                                    &&
                                                    (sort x
                                                    == fourTimes sort x))

data Fool = Fulse | Frue deriving (Eq, Show)

genFool :: Gen Fool
genFool = elements [Fulse, Frue]

genFool' :: Gen Fool
genFool' = frequency [(1, return Frue),
                      (2, return Fulse)]

runSpecs :: IO ()
runSpecs = do
  quickCheck prop_halvesNumber
  quickCheck prop_sorted
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
  quickCheck prop_mulAssociative
  quickCheck prop_mulCommutative
  quickCheck prop_expoAssociative
  quickCheck prop_expoCommutative
  quickCheck (prop_quotRem :: NonZero Integer -> NonZero Integer -> Bool)
  quickCheck (prop_divMod :: NonZero Integer -> NonZero Integer -> Bool)
  quickCheck (prop_reverse :: [Integer] -> Bool)
  quickCheck prop_dollar
  quickCheck prop_comp
  quickCheck (prop_addLists :: [Integer] -> [Integer] -> Bool)
  quickCheck (prop_concat :: [Integer] -> [Integer] -> Bool)
  quickCheck prop_length
  quickCheck (prop_readShow :: Integer -> Bool)
  quickCheck prop_capitalize
  quickCheck prop_sorted2


