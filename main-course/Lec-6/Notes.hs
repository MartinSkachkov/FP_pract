module Notes where

-- BASICS
x :: Int
x = 4

y :: Int
y = 3

inRange :: Ord a => a -> a -> a -> Bool
inRange min max x = x >= min && x <= max

inRange' :: Ord p => p -> p -> p -> Bool
inRange' min max x =
  let in_lower = min <= x
      in_upper = max >= x
   in in_lower && in_upper

inRange'' :: Ord p => p -> p -> p -> Bool
inRange'' min max x = in_lower && in_upper
  where
    in_lower = min <= x
    in_upper = max >= x

fac :: (Ord t, Num t) => t -> t
fac n
  | n <= 1 = 1
  | otherwise = n * fac (n - 1)

fac' :: (Ord t, Num t) => t -> t
fac' n =
  if n <= 1
    then 1
    else n * fac' (n - 1)

fac'' :: (Ord t, Num t) => t -> t
fac'' n = f n 1
  where
    f n acc
      | n <= 1 = acc
      | otherwise = f (n - 1) (n * acc) -- it is not bounded by the size of our stack a.k.a we can't have stack overflow in tail recursion

result :: Integer
result =
  let x = y
      y = x
   in x + y

square :: Int -> Int
square x = x * x

add1 :: Int -> Int
add1 x = x + 1

twice :: (b -> b) -> b -> b
twice f x = f $ f x

diag :: (b -> b -> a) -> b -> a
diag f x = f x x

-- HIGHER ORDER FUNCTIONS
app :: (a -> b) -> a -> b
-- app f x = f x, but this can be rewritten like this since f(x) is the same function as f(x):
app f = f

-- ghci> app (\x -> x + 1)

-- LAMBDAS
-- (\<args> -> expr)
-- (\x -> x + 1) the lambda function takes an argument x and returns the result of x + 1
-- we can assign the lambdas to variables but this is actually like a function definition

var :: Integer -> Integer
var = \x -> x + 1

var2 :: Integer -> Integer -> Integer -> Integer
var2 = \x y z -> x + y + z

-- MAP
-- to do

-- Currying
add :: Int -> Int -> Int
-- add x y = x + y
-- add x = \y -> x + y
add = \x -> (\y -> x + y)

doubleList :: [Integer] -> [Integer]
doubleList = map (\x -> x * 2) -- map function will return [a] -> [b] a.k.a will expect just a list to be passed to it
-- ghci> doubleList [1, 2, 3] => [2,4,6]
