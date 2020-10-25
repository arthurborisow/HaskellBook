module Cipher where

import Data.Char

startPos :: Int
startPos = ord 'a'


shiftChar :: Int -> Char -> Char
shiftChar n c = f $ chr newPos
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