{-# LANGUAGE EmptyDataDeriving #-}
-- cover all cases!
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}

-- warnings

module ADTs where

data Color = Orange | Yellow | Magenta

data Direction = North | West | South | East -- named value versions of Direction
-- data <TypeName> = <Value1> | <Value2> | ...
  deriving (Show, Enum) -- generate the capability for these types to be displayed the terminal and put a numeration on them

-- pattern matching
turnLeft :: Direction -> Direction
turnLeft East = North -- (case 1, the direction is East) pattern matching
turnLeft x = succ x -- (case 2, the direction is not East but sth. else (possible to use succ x thanks to deriving Enum))
-- what we pass as a direction to x will be moved one position to the right and return the direction according to the enum

turnRight :: Direction -> Direction
turnRight North = East -- (case 1, when the input is North)
turnRight x = pred x -- (case 2, when the input is sth. different from North)

turnAround :: Direction -> Direction
turnAround x = turnRight (turnRight x)

{-
ghci> turnRight (turnLeft (turnLeft West))
South
ghci> turnLeft (turnLeft (turnLeft West))
North
-}

data Answer = Yes | No | Unknown
  deriving (Show)

-- pattern matching flip' Yes -> No
flip' :: Answer -> Answer
flip' Yes = No
flip' No = Yes
flip' Unknown = Unknown

data RGBNames = Red | Green | Blue -- named value version of RGB

data RGB = RGBColor Int Int Int -- no named value version of RGB (showing how to create an object of type RGB)
-- data <TypeName> = <Constructor> <Input1> <Input2> ...
  deriving (Show)

red :: RGB
red = RGBColor 255 0 0 -- creating object of type RGB called red

green :: RGB
green = RGBColor 0 255 0

blue :: RGB
blue = RGBColor 0 0 255

addColor :: RGB -> RGB -> RGB -- creating a function receiving RGB objects and returning an RGB object
addColor (RGBColor r1 g1 b1) (RGBColor r2 g2 b2) =
  RGBColor (r1 + r2) (g1 + g2) (b1 + b2)

{-
ghci> addColor red blue
RGBColor 255 0 255

ghci> addColor (RGBColor 1 2 3) (RGBColor 2 3 4)
RGBColor 3 5 7
-}

{-
insted of deriving the Show we could make our own instance
instance Show RGBColor where
    show (RGBColor r g b) =
        "Color: " ++ (show(r, g, b))
-}

data Shape = Circle Float | Rect Float Float
  -- the constructors in data declaration can have parameters
  -- Circle and Rect can be viewed as functions that construct values of type Shape
  -- Circle :: Float -> Shape
  -- Rect :: Float -> Float -> Rect
  deriving (Show)

-- pattern matching area (Circle 2) gives some result which is Float
area :: Shape -> Float
area (Circle r) = r ^ 2 * pi
area (Rect x y) = x * y

-- we can create an object of type Expr in 3 different ways with different constructors based on what we want
data Expr
  = Lit Int -- <constructor> <input>
  | Add Expr Expr -- <constructor> <input> <input> (recursive data type)
  | Sub Expr Expr -- <constructor> <input> <input>

x :: Expr
x = Lit 5 -- we crated and object of type Expr named x with the constructor Lit and value 5

y :: Expr
y = Lit 6

sum :: Expr
sum = Add x y

diff :: Expr
diff = Sub x y

eval :: Expr -> Int
eval (Lit n) = n
eval (Add x y) = (eval x) + (eval y)
eval (Sub x y) = (eval x) - (eval y)

-- eval diff = eval (Sub (Lit 3) (Lit 4))

-- eval (Lit 5)
-- eval (Add (Lit 3) (Lit 4))
-- eval (Sub (Lit 3) (Lit 4))
-- eval (Sub (Add (Lit 4) (Lit 1)) (Lit 2))

data Calculation = Sum Int Int | Diff Int Int | Mul Int Int | Div Int Int
  deriving (Show)

-- pattern matching with the constructors
calculate :: Calculation -> Int
calculate (Sum x y) = x + y
calculate (Diff x y) = x - y
calculate (Mul x y) = x * y
calculate (Div x y) = div x y

-- crating objects of type Calculation (:t add)
add :: Calculation
add = Sum 1 2 -- calculate add -> 3 (matching the passed object with which constructor is created and then does operations on its data-members)

dif :: Calculation
dif = Diff 1 2 -- calculate dif -> -1

mul :: Calculation
mul = Mul 1 2

diV :: Calculation
diV = Div 1 2

-- recurtsive data types
data NatNum = Succ NatNum | Zero
  deriving (Show)

intToNat :: Int -> NatNum
intToNat 0 = Zero
intToNat n = Succ (intToNat (n - 1))

{-
ghci> intToNat 3
n = 3
Succ(intToNat (2))
    Succ(intToNat (1))
        Succ(intToNat (0))
            Zero -> NatNum
Succ (Succ (Succ Zero))
-}

natToInt :: NatNum -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

{-
natToInt Zero -> 0
natToInt (Succ Zero) -> 1 + natToInt Zero -> 1 + 0 -> 1

ghci> natToInt (Succ (Succ Zero))
2

natToInt (Succ (Succ Zero))
1 + natToInt (Succ Zero)
1 + 1 + natToInt Zero
1 + 1 + 0
2
-}
