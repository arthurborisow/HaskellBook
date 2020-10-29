{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chapter11 where

import Data.Char

newtype Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data PlaneSize = Small | Medium | Large deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline PlaneSize deriving (Eq, Show)

myCar :: Vehicle
myCar = Car Mini (Price 14000)

urCar :: Vehicle
urCar = Car Mazda (Price 20000)

clownCar :: Vehicle
clownCar = Car Tata (Price 7000)

doge :: Vehicle
doge = Plane PapuAir Medium

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = fmap isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _ = undefined

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where
  tooMany = tooMany . fst

instance TooMany (Int, Int) where
  tooMany = tooMany . uncurry (+)

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany = tooMany . uncurry (+)

data Fiction = Fiction deriving (Show)

data Nonfiction = Nonfiction deriving (Show)

data BookType
  = FictionBook Fiction
  | NonfictionBook Nonfiction
  deriving (Show)

type AuthorName = String

data Author = Author (AuthorName, BookType)

data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer
  = Programmer
      { os :: OperatingSystem,
        lang :: ProgLang
      }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer { os = o, lang = l } | o <- allOperatingSystems, l <- allLanguages]


data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ a : inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testTree2 :: BinaryTree Integer
testTree2 = Node (Node (Node Leaf 4 (Node (Node Leaf 6 Leaf) 5 Leaf)) 1 Leaf) 2 (Node (Node Leaf 7 Leaf) 3 Leaf)

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ z Leaf = z
foldTree f z (Node left a right) = f a (foldTree f (foldTree f z right) left)

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] [] = True
isSubseqOf _ [] = False
isSubseqOf [] _ = True
isSubseqOf xx@(x:xs) (y:ys) = if x == y
                              then isSubseqOf xs ys
                              else isSubseqOf xx ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords = fmap f . words
  where
    f [] = undefined
    f word@(x:xs) = (word, toUpper x : xs)

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph = unwords . map f . sentences
  where sentences [] = []
        sentences ss = takeWhile (/= '.') ss : sentences (dropWhile (not . isAlpha) (dropWhile (/= '.') ss))

        f (x:xs) = (toUpper x : xs) ++ "."
        f _ = undefined
