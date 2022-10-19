{-# LANGUAGE EmptyDataDeriving #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2

-- warnings

module RecursiveDataTypes where

data Nat = Succ Nat | Zero
    deriving Show

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = Succ (intToNat (n - 1))

--ghci> intToNat 3
--Succ (Succ (Succ Zero))

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + (natToInt n)

--ghci> natToInt (Succ (Succ Zero))
--2

add :: Nat -> Nat -> Nat
add x y = intToNat (natToInt x + natToInt y)

--we are doing a recursion on the left side parameter
add' :: Nat -> Nat -> Nat
add' Zero n = n --base case
add' (Succ m) n = Succ (add' m n) --recursion on the left operand

--add' (Succ     (m)    )     (n)
--add' (Succ (Succ Zero)) (Succ Zero)
-- Succ(add' (Succ Zero) (Succ Zero))
--      Succ ((add' Zero) (Succ Zero))
--                 (Succ Zero)
--Succ(Succ(Succ Zero))

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult n Zero = Zero
mult (Succ m) n = add'(mult m n) n
-- (m + 1) * n = (m * n) + n

data Expr = Val Double
          -- Val :: Double -> Expr
          | Add Expr Expr
          -- Add :: Expr -> Expr -> Expr
          | Mul Expr Expr
          -- Mul :: Expr -> Expr -> Expr
          | Div Expr Expr
          -- Div Expr -> Expr -> Expr
-- in order to use Add, Mul, Div we need first to conver Double to Expr because they accept Expr parameters
    deriving Show

calculate :: Expr -> Double
calculate (Val x) = x
calculate (Add p q) = calculate p + calculate q --calculate p runs recursively and returns an Int after that calculate q runs recursively and returns an Int and finally we add them together which returns an int
calculate (Mul p q) = calculate p * calculate q
calculate (Div p q) = (calculate p) / (calculate q)
