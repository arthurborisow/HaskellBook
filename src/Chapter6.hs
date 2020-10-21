module Chapter6 where

newtype TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (TisAn i) == (TisAn i') = i == i'

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (Two i ii) == (Two i' ii') = i == i' && ii == ii'

data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (TisAnInt i) == (TisAnInt i') = i == i'
  (TisAString s) == (TisAString s') = s == s'
  _ == _ = False

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (Pair a a') == (Pair b b') = a == b && a' == b'

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (Tuple a b) == (Tuple a' b') = a == a' && b == b'

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (ThisOne a) == (ThisOne b) = a == b
  (ThatOne a) == (ThatOne b) = a == b
  _ == _ = False

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (Hello a) == (Hello b) = a == b
  (Goodbye a) == (Goodbye b) = a == b
  _ == _ = False

newtype Person = Person Bool deriving (Show)

printPerson :: Person -> IO ()
printPerson = print

data Mood
  = Blah
  | Woot
  deriving (Show, Eq)

settleDown :: Mood -> Mood
settleDown x = if x == Woot then Blah else x

type Subject = String

type Verb = String

type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 :: Object -> Sentence
s1 = Sentence "dogs" "drool"

s2 :: Sentence
s2 = Sentence "Julie" "loves" "dogs"

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f = (==) . f

arith ::
  Num b =>
  (a -> b) ->
  Integer ->
  a ->
  b
arith f x a = fromInteger x + f a
