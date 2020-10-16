module Reverse where

rvrs :: String -> String
rvrs s = concat [first, " ",  second, " ", third]
  where
    first = drop 9 s
    second = take 2 $ drop 6 s
    third = take 5 s

main :: IO ()
main = print $ rvrs "Curry is awesome"
