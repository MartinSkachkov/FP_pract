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

module ADTs where

-- TODO: talk about

-- * pls write on teams+join teams

-- * waiting to get timetable?

-- * first homework coming soon :)

-- show:

-- * pragmas on the top of files

-- * where from last week

-- * remind about sections

-- pattern matching

-- show Bool
-- show case here?
-- deriving Show

-- analogue with C structs or something
-- example point
-- mention constructor name requirements, Mk convention
data Point = MkPoint Float Float
  deriving (Show)

isInFirstQuad :: Point -> Bool
isInFirstQuad (MkPoint x y) = (x > 0) && (y > 0) -- във функцията все едно използваме член-данните
-- isInFirstQuadrant (MkPoint 5.0 3.0)

invert :: Point -> Point
invert (MkPoint x y) = MkPoint (negate x) (negate y)

-- we put (MkPoint x y) in parenthases because it is a one whole argument otherwise the interpreter will count it as three different args : MkPoint x y
-- invert pt =
-- case pt of
-- MkPoint x y -> MkPoint (negate x) (negate y)

data MyBool = MyTrue | MyFalse
  deriving (Show)

myNot :: MyBool -> MyBool
myNot MyTrue = MyFalse
myNot MyFalse = MyTrue

-- RPS - enum example
data RPS = Rock | Paper | Scissors
  deriving (Show)

-- show ignore pattern match
-- pattern evaluation order
beats :: RPS -> RPS -> Bool
beats Rock Scissors = True
beats Scissors Paper = True
beats Paper Rock = True
beats _ _ = False

-- cats n dogs
-- colours n breeds
-- example for animal value
-- example with animal matching
-- showAnimal
data AnimalType
  = Dog Breed
  | Cat Color
  deriving (Show)

data Animal = MkAnimal AnimalName AnimalType

data Color = Orange | Black | White
  deriving (Show)

data Breed = Labrador | Husky | Borzoi
  deriving (Show)

showAnimal :: Animal -> String
showAnimal (Dog Borzoi) = "weird"
showAnimal (Cat Orange) = "orange cat"
showAnimal animal =
  case animal of
    (Dog Husky) -> "husky"
    (Cat Black) -> "black cat"
    (Dog Labrador) -> "labrador"
    (Cat White) -> "white cat"

-- explain the encoding (peano)
-- recursive data types
data Nat = Zero | Succ Nat
  deriving (Show)

-- implement
integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat n = (Succ (integerToNat (n - 1)))

-- evaluate manually?
-- n = 3
-- (Succ (integerToNat (2)))
--        (Succ (integerToNat(1)))
--               (Succ (integerToNat(0)))
--                      (Zero)

-- implement
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + (natToInteger n)

-- evaluate manually?
-- (Succ (Succ (Succ Zero)))
-- 1 + (natToInteger (Succ (Succ Zero)))
-- 1 + 1 + (natToInteger (Succ Zero))
-- 1 + 1 + 1 + (natToInteger Zero)
-- 1 + 1 + 1 + 0
-- 3

-- implement
addNat :: Nat -> Nat -> Nat
addNat Zero n = n
addNat (Succ m) n = (Succ (addNat m n))

-- evaluate manually?
-- Zero (Succ Zero) -> (Succ Zero)
-- (Succ (Succ Zero)) (Succ Zero)
-- (Succ (addNat (Succ Zero) (Succ Zero)))
--       (Succ (addNat Zero) (Succ Zero))
--              (Succ Zero)
-- (Succ (Succ (Succ Zero)))

-- test
test :: Nat -> Nat
test (Succ x) = x
test x = x

-- depending which one is first the result is different as (Succ sth.) is pattern matching and it removes the first Succ and returns sth.
-- while test x returns the same x (non-pattern matching)

-- TASK
-- define what the "next" throw you can do is in the "usual" ordering of RPS
-- i.e. @next x@ should be the throw that beats x
next :: RPS -> RPS
next Scissors = Rock
next Rock = Paper
next Paper = Scissors

-- TASK
-- define what it means for two RPS values to be equal
-- in general for a type, this would mean that the constructors must be equal
-- and all their contents should all so be equal (pointwise)
-- for an "enum" in particular, this only leaves the constructor check
-- use _ matches!
-- EXAMPLES
-- >>> eqRPS Rock Rock
-- True
-- >>> eqRPS Rock Paper
-- False

eqRPS :: RPS -> RPS -> Bool
eqRPS Rock Rock = True
eqRPS Scissors Scissors = True
eqRPS Paper Paper = True
eqRPS _ _ = False

-- TASK
-- define a shorter version of beats by uisng next and eqRPS
-- EXAMPLES
-- >>> beats' Rock Paper
-- True
-- >>> beats' Rock Scissors
-- False
-- >>> beats' Paper Scissors
-- True
beats' :: RPS -> RPS -> Bool
beats' x y = eqRPS x (next y)

-- TASK
-- Your task is to model a few of the pieces of the game of Belote

-- * implement a data type for Ranks (7 8 9 10 J etc)

data Rank = Ace | King | Queen | Jack | Num Int
  deriving (Show)

-- * implement a data type for Suits

data Suit = Clubs | Diamonds | Hearths | Spades
  deriving (Show)

-- * implement a data type for a Card

data Card = MkCard Rank Suit
  deriving (Show)

-- * implement a data type for Contracts (all trump, no trump etc)

data Contract = AllTrump | NoTrump
  deriving (Show)

-- Given a Card and a Contract, implement a check whether the card is of a trump suit
-- done only for all trump
-- J; 9; A; 10; K; Q; 8; 7.
isTrump :: Contract -> Card -> Bool
isTrump AllTrump (MkCard Jack Diamonds) = True
isTrump AllTrump (MkCard (Num 9) Diamonds) = True
isTrump AllTrump (MkCard Ace Diamonds) = True
isTrump AllTrump (MkCard (Num 10) Diamonds) = True
isTrump AllTrump (MkCard King Diamonds) = True
isTrump AllTrump (MkCard Queen Diamonds) = True
isTrump AllTrump (MkCard (Num 8) Diamonds) = True
isTrump AllTrump (MkCard (Num 7) Diamonds) = True
isTrump _ _ = False

-- Given a Card and a Contract, implement what the "power" of that card is as an Integer
-- When played, a card with higher power will "beat" one with lower power
-- You can use whatever numbers you like, as long as they reflect the rules of the game.
-- done only for all trump
cardPower :: Contract -> Card -> Integer
cardPower AllTrump (MkCard King Diamonds) = 4
cardPower AllTrump (MkCard Ace Diamonds) = 11
cardPower AllTrump (MkCard Queen Diamonds) = 3
cardPower AllTrump (MkCard Jack Diamonds) = 20
cardPower AllTrump (MkCard (Num 10) Diamonds) = 10
cardPower AllTrump (MkCard (Num 9) Diamonds) = 14
cardPower _ _ = 0

-- Given two Cards and a Contract, return the Card that would "win" (according to their power)
-- when playing under the given Contract
-- Assume that the first matched card is played first.
fight :: Contract -> Card -> Card -> Card
fight AllTrump (MkCard (Num 9) Diamonds) (MkCard Jack Diamonds) = (MkCard Jack Diamonds)

-- ...

-- TASK
-- multiply two @Nat@s recursively, much like we did with Ints last time
-- EXAMPLES
-- >>> multNat Zero (Suc (Suc (Suc Zero)))
-- Zero
-- >>> multNat (integerToNat 2) (integerToNat 3)
-- Suc (Suc (Suc (Suc (Suc (Suc Zero)))))
multNat :: Nat -> Nat -> Nat
multNat Zero m = Zero
multNat n Zero = Zero
multNat (Succ n) m = addNat (multNat n m) m

-- TASK
-- calculate the larger of two @Nat@s recursively
-- EXAMPLES
-- >>> maxNat (Suc Zero) Zero
-- Suc Zero
-- >>> maxNat (Suc (Suc Zero)) Zero
-- Suc (Suc Zero)
-- >>> maxNat (Suc (Suc Zero)) (Suc (Suc (Suc (Suc Zero))))
-- Suc (Suc (Suc (Suc Zero)))
maxNat :: Nat -> Nat -> Nat
maxNat x Zero = x
maxNat Zero y = y
maxNat (Succ x) (Succ y) = Succ (maxNat x y) -- we decrement both sides with one each time and the first that reaches Zero is the smaller one so we return the bigger one

-- TASK
-- Ordering is a datatype that is made to mean "the result of a comparison" or "the ordering between two things"
-- it's defined like so:
-- @data Ordering = LT | EQ | GT@
-- with the constructors being L(ess)T(han), EQ(ual) G(reater)T(han)
-- implement a comparison for @Nat@s, returning an @Ordering@
-- EXAMPLES
-- >>> compareNat (Suc Zero) (Suc Zero)
-- EQ
-- >>> compareNat Zero (Suc Zero)
-- LT
-- >>> compareNat (Suc Zero) Zero
-- GT

compareNat :: Nat -> Nat -> Ordering
compareNat Zero Zero = EQ
compareNat Zero y = LT
compareNat x Zero = GT
compareNat (Succ x) (Succ y) = compareNat x y

-- README
-- the "syntax" for a very basic "calculator" datatype
-- or alternatively, a very simple programming language
--
-- we can build up Expr(essions) by

-- * injecting integers as a value directly - Val

-- * stating that we want to add the result of two calculations - Plus

-- * stating that we want to multiply the result of two calculations - Mult

data Expr
  = Val Integer
  | Plus Expr Expr
  | Mult Expr Expr
  | If Expr Expr Expr
  deriving (Show)

-- Plus (Val 3) (Mult (Val 4) (Val 5))
--
-- 3 + if ... then ... else ..
-- Plus (Val 3) (If ....)
-- README - SECTIONS
--
-- Writing
-- Plus (Val 3) (Plus (Val 4) (Val 5))
-- is annoying - a lot of parens
-- we can abuse sections to write "prettier" expressions
-- Val 3 `Plus` Val 4
-- is the same as
-- Plus (Val 3) (Val 4)
-- But what would
-- Val 3 `Plus` Val 4 `Plus` Val 5
-- be?
-- We can use these pragmas
infixr 7 `Plus`

infixr 8 `Mult`

-- 5 `Plus` 3 `Mult` 9
-- infixr(ight)
-- to tell the compiler that when used in a section/as operators
-- Mult has higher priority than Plus, e.g.
-- Val 3 `Plus` Val 4 `Mult` Val 5
-- means
-- Val 3 `Plus` (Val 4 `Mult` Val 5)
-- and
-- Plus and Mult are both right associative, e.g.
-- Val 3 `Plus` Val 4 `Plus` Val 5
-- means
-- Val 3 `Plus` (Val 4 `Plus` Val 5)

-- PATH?
-- chocolatey installs where
-- ghcup installs where
-- how to set vscode path

-- TASK
-- and now that we have the syntactic structure of the computation we want to make
-- we can implement its semantics by writing an evaluator for our calculator
-- or alternatively an interpreter for our programming language
-- EXAMPLES
-- >>> eval (Val 3)
-- 3
-- >>> eval (Plus (Val 3) (Val 4))
-- 7
-- >>> eval (Val 33 `Mult` Val 36)
-- 1188
-- >>> eval ((Val 3 `Plus` Val 3) `Mult` Val 7)
-- 42
-- >>> eval (Val 3 `Plus` Val 3 `Mult` Val 7)
-- 24
eval :: Expr -> Integer
eval (Val x) = x
eval (Plus x y) = eval x + eval y
eval (Mult x y) = eval x * eval y
eval (If x y z) =
  if (eval x /= 0)
    then eval y
    else eval z

-- eval (If <expr> /=  0)
--      then execute this
--      else execute this
--

-- TASK
-- add an If expression to our Expr language
-- by using other calculations for our "condition value"
-- extend eval so that it also works with the new If construction
-- interpreting 0 as "false" and any other value as "true"

-- TASK
-- add a name to the Animal type
data AnimalName = MkName String
  deriving (Show)

animalName = MkName "Marto"

-- is there more than one way to add it to Animal?
-- which way would be more convenient for the implementation of the following function?
-- introduce :: Animal -> String
-- introduce (Add (MkName x)) = x
-- introduce _ = ""
-- which shows the name of the animal