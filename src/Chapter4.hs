module Chapter4 where

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x = if x > 0 then x else negate x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

-- Correcting syntax

x' :: Int -> Int -> Int
x' = (+)

f' :: [a] -> Int
f' xs = w `x'` 1
  where w = length xs

myId :: a -> a
myId x = x

myF :: (a, b) -> a
myF (a, _) = a
