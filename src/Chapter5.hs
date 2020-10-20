module Chapter5 where

ff :: a -> a -> a
ff qq _ = qq

ff' :: a -> a -> a
ff' = flip ff

fff :: a -> b -> b
fff = flip const

bigNum :: Integer -> Integer
bigNum = (^) 5

wahoo :: Integer
wahoo = bigNum 10

x :: String -> IO ()
x = print

y :: IO ()
y = print "woohoo!"

z :: IO ()
z = x "hello world"

a :: Num a => a -> a -> a
a = (+)

b :: Num a => a
b = 5

c :: Num a => a -> a
c = a 10

d :: Num a => a
d = c 200

a' :: Num a => a
a' = 12 + b'

b' :: Num a => a
b' = 10000 * c'

c' :: Num a => a
c' = 12

functionH :: [a] -> a
functionH (x_ : _) = x_
functionH _ = undefined

functionC :: Ord a => a -> a -> Bool
functionC = (>)

functionS :: (a, b) -> b
functionS = snd

i :: a -> a
i = id

c''' :: a -> b -> a
c''' = const

c'' :: b -> a -> b
c'' = const

cc' :: a -> b -> b
cc' = flip const

r :: [a] -> [a]
r = id

r' :: [a] -> [a]
r' = tail

co :: (b -> c) -> (a -> b) -> a -> c
co b2c a2b = b2c . a2b

aa :: (a -> c) -> a -> a
aa = flip const

aa' :: (a -> b) -> a -> b
aa' = ($)

-- module Sing where -- in file Sing.hs
fstString :: String -> String
fstString = (++ " in the rain")

sndString :: String -> String
sndString = (++ " over the rainbow")

sing :: String
sing = if x_ > y_ then fstString x_ else sndString y_
  where
    x_ = "Singin"
    y_ = "Somewhere"

-- module Arith3Broken where -- in fileArith3Broken.hs
main :: IO ()
main = do
  print (1 + 2 :: Integer)
  putStrLn "10"
  print (negate (-1) :: Integer)
  print ((+) 0 blah)
  where
    blah = negate 1 :: Integer

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h = g . f

data A

data B

data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q

data X

data Y

data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (xxx, yyy) = (xz xxx, yz yyy)

munge ::
  (x -> y) ->
  (y -> (w, z)) ->
  x ->
  w
munge x2y y2wz = fst . y2wz . x2y
