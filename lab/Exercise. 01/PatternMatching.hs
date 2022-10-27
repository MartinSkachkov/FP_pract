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

module PatternMatching where

spell :: Int -> String
spell x =
  case x of -- case <expr> of
    1 -> "one" --     <pattern> -> <expr>
    2 -> "two" --     <pattern> -> <expr>
    _ -> "number" -- default             --      ...

is_zero :: Int -> Bool
is_zero 0 = True
is_zero _ = False

-- pattern matching & recursion
factorial :: Int -> Int
factorial 0 = 1 -- (case 1)
factorial n = n * factorial (n - 1) -- (case 2)
-- name <pattern> = do sth...

-- non-pattern matching & recursion
fact :: Int -> Int
fact n =
  if (n == 0)
    then 1
    else n * fact (n - 1)

-- non-pattern matching
add43or1337 :: Bool -> Int -> Int
add43or1337 shouldAdd43 x =
  x
    + if shouldAdd43
      then 43
      else 1337

-- pattern-matching
-- Each case branch can deconstruct the different constructors of a union type
add43or1337' :: Bool -> Int -> Int
add43or1337' shouldAdd43 x =
  x + case shouldAdd43 of
    True -> 43
    False -> 1337

-- another way
add42or1337'' :: Bool -> Int -> Int
add42or1337'' True x = x + 42
add42or1337'' False x = x + 1337

-- GUARDS (kind of similar to the pattern matching) (sth. like if...else if...else)
max' :: Int -> Int -> Int
max' x y
  | x > y = x
  | otherwise = y

fact' :: Int -> Int
fact' n
  | n == 0 = 1
  | otherwise = n * fact' (n - 1)