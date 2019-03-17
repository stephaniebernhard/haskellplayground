import Data.List
import Data.Set

-- Aufgabe 1
data BTree a
    = BNode (BTree a) a (BTree a)
    | Leaf a
    deriving (Show, Eq)

collect :: BTree a -> [a]
collect (Leaf a) = [a]
collect (BNode left a right) =
    (collect left)
    ++ [a]
    ++ (collect right)

bTree :: BTree Integer
bTree = BNode (Leaf 3) 2 (Leaf 1)
-- i.e. collect bTree = [3,2,1]
-- i.e. b2Tree = BNode (Leaf 1) 4 (BNode (Leaf 4) 0 (Leaf 9))
--      collect b2Tree = [1,4,4,0,9]

data Tree a = Node a [Tree a]
    deriving (Show, Eq)

aTree :: Tree Integer
aTree = Node 5
    [ Node 4 []
    , Node 3
        [ Node 2 []
        , Node 1 []
        ]
    , Node 6
        [ Node 0 []
        ]
    ]

-- Aufgabe 2
data NatNumber = Zero | Succ NatNumber
    deriving (Show, Eq)

eval :: NatNumber -> Integer
eval Zero = 0
eval (Succ n) = eval(n) + 1
-- i.e. eval (Succ Zero) = 1
--      eval Zero = 0
--      eval (Succ (Succ Zero)) = 2 etc.

interpret :: Integer -> NatNumber
interpret 0 = Zero
-- interpret n = (Succ (interpret (n-1)))
interpret n = Succ $ interpret $ n-1
-- $ eliminates the use of ()
-- $ sets () around whats followed by $
-- i.e interpret 4 =
--     Succ (Succ (Succ (Succ Zero)))

add :: NatNumber -> NatNumber -> NatNumber
-- add a b = interpret $ (eval a) + (eval b)
add n Zero = n
add n (Succ m) = add (Succ n) m
-- i.e. add (Succ Zero) (Succ Zero)

multiply :: NatNumber -> NatNumber -> NatNumber
-- multiply a b = interpret $ (eval a) * (eval b)
multiply n Zero = Zero
multiply n (Succ m) = add (multiply n m) n
-- i.e. eval (multiply (interpret 3) (interpret 4))

fact :: NatNumber -> NatNumber
fact (Zero) = Succ Zero
fact (Succ n) = multiply (Succ n) (fact n)
-- i.e. fact( Succ (Succ (Succ(Zero)))) 

-- Aufgabe 3
-- FRACTRAN is a Turing-complete esoteric programming language
-- invented by the mathematician John Conway. 
-- A FRACTRAN program is an ordered list of positive fractions
-- together with an initial positive integer input n. 
-- The program is run by updating the integer n as follows: 
-- for the first fraction f in the list for which nf is 
-- an integer, replace n by nf repeat this rule until no 
-- fraction in the list produces an integer when multiplied 
-- by n, then halt. 

-- Aufgabe 4
newtype Model = Model String
    deriving (Show, Eq)

newtype Make = Make String
    deriving (Show, Eq)

data Color = Color
    { red :: Int
    , green :: Int
    , blue :: Int
    } deriving (Show, Eq)

redColor = Color 255 0 0
greenColor = Color 0 255 0
whiteColor = Color 255 255 255

data Horsepower = Horsepower Int
    deriving (Show, Eq, Ord)

data Car = Car
    { model :: Model
    , make :: Make
    , year :: Integer
    , color :: Color
    , power :: Horsepower
    } deriving (Show, Eq)

ford :: Car
ford = Car 
    { model = Model "Fiesta"
    , make = Make "Ford"
    , year = 2017
    , color = redColor
    , power = Horsepower 70
    }

ferrari :: Car
ferrari = Car
    { model = Model "Unicorn"
    , make = Make "Ferrari"
    , year = 2000
    , color = greenColor
    , power = Horsepower 50
    }

zoe :: Car
zoe = Car
    { model = Model "Zoe"
    , make = Make "Renault"
    , year = 1989
    , color = whiteColor
    , power = Horsepower 20
    }

instance Ord Car where
    compare (Car _ _ _ _ power1) (Car _ _ _ _ power2) = compare power1 power2

-- i.e. sort [zoe, ferrari, ford] yields
-- [Car {model = Model "Zoe", make = Make "Renault", year = 1989, 
-- color = Color {red = 255, green = 255, blue = 255}, power = Horsepower 20},
-- Car {model = Model "Unicorn", make = Make "Ferrari", year = 2000, 
-- color = Color {red = 0, green = 255, blue = 0}, power = Horsepower 50},
-- Car {model = Model "Fiesta", make = Make "Ford", year = 2017, 
-- color = Color {red = 255, green = 0, blue = 0}, power = Horsepower 70}]