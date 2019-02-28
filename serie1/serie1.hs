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
substring :: String -> String -> Bool
substring x y = take (length x) y == x
-- i.e. substring "abc" "abde"
-- i.e. substring "abc" "abcde"

-- AUFGABE 4
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)
-- i.e. replace "hello" "he" "!!" 

parens :: String -> String
parens x = replace x "a" "<a>"

parens2 :: String -> String
parens2 x = replace x "abc" "<abc>"
