module Chapter3 where

addBang :: String -> String
addBang s = s ++ "!"

getFifthChar :: String -> Char
getFifthChar x = x !! 4

returnAwesome :: String -> String
returnAwesome = drop 9

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex i = let s = "Curry is awesome!"
                in s !! i
