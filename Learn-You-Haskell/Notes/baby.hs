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

-- ghci
-- >:l FirstFunc.hs (load the functions)
-- >:r (if something in the file is changed, reload it)

-- Important!
-- negative numbers are always surrounded by parentheses
-- logical operators: &&, ||, not
-- equality: ==, /=
-- we can do 5 + 4.0

-- this function work with int type arguments as well as double type args(basically a number)
doubleMe :: Num a => a -> a -- Num a, restricts the possible types for the a's (they can be only numbers due to the fact that '+' operator works only with numbers)
doubleMe x = x + x

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

setOrder :: [a] -> [b] -> [(a, b)]
setOrder = zip

-- setOrder numbers xs = zip numbers xs

-- <name> <args> = <body>
-- calling the function: doubleMe 3

-- doubleMe :: Int -> Int --only works with int arguments passed not with double (doubleMe 4.5 -> error)
-- doubleMe x = x + x

-- doubleUs :: Double -> Double -> Double --works well with int (as it can be cast to double 5.0) and double
-- doubleUs x y = x*2 + y*2

doubleUs x y = doubleMe x + doubleMe y

-- functions in Haskell don't have to be in any particular order, so it doesn't matter if you define doubleMe first and then doubleUs or if you do it the other way around.
-- doubleUs 3 4 or we can call it like that 3 `doubleUs` 4

doubleSmallNumber x =
  if x > 100
    then x -- true
    else doubleMe x -- false

doubleSmallNumber' x = (if x > 100 then x else doubleMe x) + 1 -- the (if...) will return a value since it is an expression and the value will be incremented by one
-- expression : part of the code that returns a value(ex: if, x + y...)
-- it's a valid character to use ' in funcion's name

super'Mario = "It's me, Mario!"

-- function names can't start with upper letter
-- function without parameters is called definition

-- Function application (calling a function by putting a space after it and then typing out the parameters) has the highest precedence of them all
-- succ 9 + 5 + max 2 4 is the same as:
-- (succ 9) + 5 + (max 2 4)
-- succ 9 * 10 -> 100 since the function succ will be executed first