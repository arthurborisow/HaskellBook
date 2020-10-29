module Chapter11.Phone where

import Data.List
import Data.Char

-- -----------------------------------------

-- |   1        |    2 ABC    |   3 DEF    |
-- _________________________________________
-- |   4 GHI    |    5 JKL    |   6 MNO    |
-- -----------------------------------------
-- |   7 PQRS   |    8 TUV    |   9 WXYZ   |
-- -----------------------------------------
-- |   * ^      |    0 + _    |   # .,     |
-- ----------------------------------------
newtype DaPhone = DaPhone [Symbol] deriving (Show, Eq)
data Symbol = Symbol Char Digit Presses deriving (Show, Eq)

mkPhone :: [(Digit, String)] -> DaPhone
mkPhone = DaPhone . concatMap f
  where f (c, path) = zipWith (\x y -> Symbol x c y) (path ++ [c]) [1..]

phone :: DaPhone
phone =
  mkPhone
    [ ('1', ""),
      ('2', "abc"),
      ('3', "def"),
      ('4', "ghi"),
      ('5', "jkl"),
      ('6', "mno"),
      ('7', "pqrs"),
      ('8', "tuv"),
      ('9', "wxyz"),
      ('*', "^"),
      ('0', "+_"),
      ('#', ".,")
    ]

convo :: [String]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"
  ]

type Digit = Char
type Presses = Int

findSymbol :: DaPhone -> Char -> Maybe Symbol
findSymbol (DaPhone symbols) c = find (\(Symbol x _ _) -> x == c) symbols

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps p c = fmap f chars
  where chars = if isUpper c then ['*', toLower c] else [c]
        f ch = case findSymbol p ch of
                 Just (Symbol _ y z) -> (y, z)
                 Nothing -> ('!', 0)

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead = concatMap . reverseTaps

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr ((+) . snd) 0

mostPopularLetter :: String -> Char
mostPopularLetter = head . maximumBy (\x y -> length x `compare` length y) . group . sort

letterCost :: DaPhone -> Char -> Presses
letterCost p = fingerTaps . reverseTaps p

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . unlines

coolestWord :: [String] -> String
coolestWord = head . maximumBy (\x y -> length x `compare` length y) . group  . sort . words .  unlines
