module Cipher where

import Data.Char
import Test.QuickCheck

startPos :: Int
startPos = ord 'a'


shiftChar :: Int -> Char -> Char
shiftChar n c = if isAlpha c then f $ chr newPos else c
  where
    char = toLower c
    shift = ord char + n - startPos
    newPos = startPos + mod shift 26
    f :: Char -> Char
    f
      | isUpper c = toUpper
      | otherwise = id

caesar :: Int -> String -> String
caesar = fmap . shiftChar

unCaesar :: Int -> String -> String
unCaesar = caesar . negate

newtype Secret = Secret String deriving (Show, Eq)

_vigCiph :: (Int -> Int) -> Secret -> String -> String
_vigCiph shiftNT (Secret secret) s = map f pairs
                                       where
                                         newString = substituteString (cycle secret) s
                                         pairs = zip s (map toLower newString)
                                         f :: (Char, Char) -> Char
                                         f (sc, ss) = shiftChar (shiftNT (currentPos - startPos)) sc
                                           where currentPos = ord $ toLower ss

vigenere :: Secret -> String -> String
vigenere = _vigCiph id

unVigenere :: Secret -> String -> String
unVigenere = _vigCiph negate

substituteString :: [Char] -> String -> String
substituteString [] _ = []
substituteString _ [] = []
substituteString s@(x:xs) (y:ys)
  | isAlpha y = x : substituteString xs ys
  | otherwise = y : substituteString s ys

genSecret :: Gen [Char]
genSecret =
  sized $
    \n -> do
      k <- choose (1, n)
      sequence [ arbitrary | _ <- [1..k] ]

prop_caesar :: Int -> ASCIIString -> Bool
prop_caesar n (ASCIIString x) = unCaesar n (caesar n x) == x

prop_vigenere :: ASCIIString -> Property
prop_vigenere (ASCIIString x) = forAll genSecret (\s -> unVigenere (Secret s) (vigenere (Secret s) x) == x)

checkCaesar :: IO ()
checkCaesar = quickCheck prop_caesar

checkVigenere :: IO ()
checkVigenere = quickCheck prop_vigenere
