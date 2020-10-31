module Chapter13 where

import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (isAlpha, toLower)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case isPalindrom line1 of
    True -> putStrLn "It's a palindrome!"
    False -> exitSuccess

isPalindrom :: String -> Bool
isPalindrom s = ss == reverse ss
  where ss = map toLower $ filter isAlpha s

type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String deriving (Eq, Show)
mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | age <= 0 = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Please enter name: "
  name <- getLine
  putStrLn ""
  putStr "Please enter age: "
  age <- getLine

  case mkPerson name (read age) of
    Right p -> putStrLn $ "Yay! Successfully got a person: " ++ show p
    Left e -> print e
