-------------------------------------------------------------------------------
-- | Auxiliary list and string functions
-------------------------------------------------------------------------------

import Data.List

-- | All partitions of a List in two parts.
part :: [a] -> [([a],[a])]
part xs = map (\n -> (take n xs, drop n xs)) [1 .. l]
    where
        l = length xs

-------------------------------------------------------------------------------
-- | Syntax
-------------------------------------------------------------------------------

-- | The abstract syntax tree (AST) of regular expressions
data Regex
    = Empty
    | Epsilon
    | Symbol Char
    | Concat [Regex]
    | Or [Regex]
    | Plus Regex
    deriving (Show, Eq)

-- | The catamorphism matching the AST
regex
    :: a
    -> a
    -> (Char -> a)
    -> ([a] -> a)
    -> ([a] -> a)
    -> (a -> a)
    -> Regex
    -> a
regex em ep sym con choice plus expression =
    case expression of
        Empty -> em
        Epsilon -> ep
        Symbol s -> sym s
        Concat rs -> con $ map recurse rs
        Or rs -> choice $ map recurse rs
        Plus r -> plus $ recurse r
    where
        recurse = regex em ep sym con choice plus

-------------------------------------------------------------------------------
-- | Makros i.e. syntax extensions
-------------------------------------------------------------------------------

-- | Kleene's star
star :: Regex -> Regex
star r = Or [Epsilon, Plus r]

-- | One or zero repetitions
maybe :: Regex -> Regex
maybe r = Or [Epsilon, r]

-- | Exactly n repetitions
times :: Int -> Regex -> Regex
times n r = Concat $ take n $ repeat r 

-------------------------------------------------------------------------------
-- | Semantics of regular expressions in terms of String predicates
-------------------------------------------------------------------------------

eval :: Regex -> (String -> Bool)
eval = regex em ep sym serial choice plus
    where
        em = const False
        ep = (==) ""
        sym c = (==) "c"
        choice fs s = or $ map (\f -> f s) fs
        serial :: [String -> Bool] -> String -> Bool
        serial [] "" = True
        serial (f:fs) "" = f "" && serial fs ""
        serial [] _ = False
        serial (f:fs) str = 
            or
            $ map (\(x,y) -> f x && serial fs y)
            $ part str
        
        plus :: (String -> Bool) -> String -> Bool
        plus f str =
            or
            $ map (\(s, t) -> f s && (t == "" || plus f t))
            $ part str
                
-------------------------------------------------------------------------------
-- | Examples
-------------------------------------------------------------------------------

-- | a
ra :: Regex
ra = Symbol 'a'

-- | b
rb :: Regex
rb = Symbol 'b'

-- | c
rc :: Regex
rc = Symbol 'c'

-- | (a|b)
avb :: Regex
avb = Or [ra, rb]

-- | x
rx :: Regex
rx = Symbol 'x'

-- | y
ry :: Regex
ry = Symbol 'y'

-- | (x|y)
xvy :: Regex
xvy = Or [rx, ry]

-- | (x|y)(a|b)
xyCab :: Regex
xyCab = Concat [xvy,  avb]

-- | (x|y)*
xyStar :: Regex
xyStar = star xvy

-- | r = (a|b)*abc(x|y)
exRegex :: Regex
exRegex = Concat [star avb, ra, rb, rc, xvy]