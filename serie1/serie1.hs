-- AUFGABE 1a
square :: Integer -> Integer
square x = x * x
-- i.e. square -3

-- AUFGABE 1b
exp' :: Integer -> Integer
exp' x = 3^x
-- i.e. exp' 2

-- AUFGABE 1c
makePalindrome :: String -> String
makePalindrome w = w  ++ (reverse w)
-- i.e. makePalindrome "ken"

-- AUFGABE 1d
simplesum' :: (Integer -> Integer) -> Integer -> Integer
simplesum' f 1 = f 1
simplesum' f x = f x + simplesum' f (x-1)
-- i.e. simplesum' square 3

sum' :: (Integer -> Integer) -> Integer -> Integer -> Integer
sum' f m n = (simplesum' f n) - (simplesum' f (m-1))
-- i.e. sum' square 3 5

-- AUFGABE 2
sumSq :: Integer -> Integer -> Integer
sumSq m n = sum' square m n
-- i.e. sum' square 3 5

-- AUFGABE 3
-- substring :: String -> String -> Bool
-- substring = 

-- AUFGABE 4
