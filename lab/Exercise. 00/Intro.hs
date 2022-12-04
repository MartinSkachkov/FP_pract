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

module Intro where

---------------------------------

-- TODO: me
-- Georgi Lyubenov, he/him, contact info in readme
-- chaos, tweag

-- TODO: administrivia - georgi look at README!
-- live for now
-- ask about git knowledge - 3
-- github repo - https://github.com/googleson78/fp-lab-2022-23

-- TODO: exposition
--
-- throw away most of the terminology you know from other languages
--
--   (Typed) Functional Programming is
--     - Defining Datatypes To Represent Problems
--     - Defining Functions To Create New Data From Old

--
-- garbage collection
-- expressive static types
-- no null everywhere!
-- sum types
-- pattern matching!
-- functions! higher-order functions
-- lazy - a boon and a curse
-- no arbitrary IO
-- no mutations
-- value/"pipeline" oriented (give bash example maybe)
-- concise, very modular

-- all in all -
-- the language allows you to

-- * make it harder for yourself to write garbage

--   * very enjoyable to work with - I'm dumb and so I let my compiler do most of my work for me
--   * used for things that **really** shouldn't break :) - e.g. banks (standard chartered), military :(

-- * be free in what level of abstraction you want to work on

--   * you can write rather low level C-like code
--   * but you can also almost directly express some mathematical concepts in it
--

-- disadvantages
-- not ultra popular:

-- * harder to get a job

-- * some libraries might be outdated/not extremely optimised

-- * there aren't obvious "best ways" to do things sometimes

-- learning curve is very steep at the beginning - especially when you are coming from an "imperative" and/or untyped background
-- used a lot for things of questionable morality

-- myths
-- not a silver bullet
-- it's still a tool, and tools require proper usage (https://pbs.twimg.com/media/E7Kc0OhVUAAV0xz?format=jpg&name=small)
-- monads aren't hard (in haskell), they're only scary sounding

-- TODO: syntax and values
-- ghci - interactive development
-- calling functions
-- function definition
-- type declarations

-- base types
-- if
-- operators
-- holes
-- show where

-- show vscode install and features:
-- vscode examples
-- warning
f :: Int -> Int
f x = 10 + 3

-- hint
g :: Int -> Int
g = succ

-- type inference and hover
-- >>> [1,2,3] ++ [4,5]
-- [1,2,3,4,5]
h :: Int -> Int
h x = g (f x) -- first calls (f x) which return 13 and then g which succ 13
-- evaluate code in >>>
-- it can also enable extensions and do automatic imports, but we won't be lookin at that for now

-- >>> fact 5
-- 120
-- >>> fact 7
-- 5040
fact :: Int -> Int
fact n =
  if n == 0
    then 1
    else n * fact (n - 1)

-- EXAMPLES
-- >>> fib 0
-- 1
-- >>> fib 4
-- 5
-- >>> fib 8
-- 34
fib :: Integer -> Integer
fib x =
  if (x == 0 || x == 1)
    then x
    else fib (x - 1) + fib (x - 2)

-- use the following "mathematical definition" to implement addition on natural numbers:
-- myPlus x y = y                 if x == 0
-- myPlus x y = succ(myPlus(pred(x), y)) else
-- Note that succ and pred are functions that already exist
-- succ x = x + 1
-- pred x = x - 1
-- EXAMPLES
-- >>> myPlus 50 19
-- 69
-- >>> myPlus 0 42
-- 42
myPlus :: Integer -> Integer -> Integer
myPlus n m =
  if (n == 0)
    then m -- 0 + m = m
    else succ (myPlus (pred n) m)

-- same as above, implement multiplication on natural numbers recursively, using addition instead of succ
-- EXAMPLES
-- >>> myMult 3 23
-- 69
-- >>> myMult 0 42
-- 0
-- >>> myMult 1 42
-- 42
myMult :: Integer -> Integer -> Integer
myMult n m =
  if (n == 0)
    then 0
    else myPlus m (myMult (pred n) m)

-- Author's note: I factored this out to use it in both fastPow and isPrime
-- >>> 7 `divides` 42
-- True
-- >>> 5 `divides` 42
-- False
-- divides :: Integer -> Integer -> Bool
-- divides n m =
--    m `rem` n == 0

-- In the case of the exponent being even, use the property that
-- x^(2*n) == (x*x)^n
-- to implement "fast" exponentiation.
-- In case it's not, you can proceed as normally when doing exponentiation.
-- This is "fast" in the sense that it takes ~log(n) operations instead of ~n operations, where n is the exponent.
-- EXAMPLES
-- >>> fastPow 3 4
-- 81
-- >>> fastPow 2 6
-- 64
fastPow :: Integer -> Integer -> Integer
fastPow base expon =
  if (expon == 0)
    then 1 -- base^0 = 1
    else
      if (2 `divides` expon)
        then fastPow (base * base) (expon `div` 2) -- 2^4 = fastPow 2*2 2
        else base * fastPow base (pred expon) -- 2^5 = 2 * 2^4
        -- You can use the rem function:
        -- rem x y == what's the remainder of x when divided by y
        --
        -- For this task, use `where` construct(explained below) to define two helper functions:
        --

divides :: Integer -> Integer -> Bool
divides x y =
  y `rem` x == 0

--
-- anyDividesInRange :: Integer -> Integer -> Bool
-- anyDividesInRange a b == check if in the range (a,b), any of the numbers divide n. You can do this by "iterating" via recursion
--
-- `where` reminder!
-- isPrime n = ...
--   where
--     myHelperValue :: Integer
--     myHelperValue = n * 10
--
--     theHelperOfMe = myHelperValue * 10
--
--     myOtherHelper :: Integer -> Integer
--     myOtherHelper x = x + n
-- Note that the `n` variable bound in the isPrime declaration is visible over the entire body of the where statement.
-- As you can see, the helper bindings in the where block **must** be indented (and they must all be at the same indentation level)
-- EXAMPLES
-- >>> isPrime 5
-- True
-- >>> isPrime 6
-- False
-- >>> isPrime 13
-- True
isPrime :: Integer -> Bool
isPrime n = not (anyDividesInRange n 2 (pred n))

-- iterate over all the values in the given range, and check if any of them divide n
anyDividesInRange :: Integer -> Integer -> Integer -> Bool
anyDividesInRange n start end =
  if start > end
    then False
    else start `divides` n || anyDividesInRange n (succ start) end