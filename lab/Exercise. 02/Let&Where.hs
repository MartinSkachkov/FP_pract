module LetNWhere where

-- let
-- <local definition> **visible only in the scope of the current function**
-- in
-- <expr>

inRange :: Int -> Int -> Int -> Bool
inRange a b x =
  let inLowerBound = x >= a
      inUpperBound = x <= b
   in inLowerBound && inUpperBound

circ :: Double -> Double
circ r =
  let pi = 3.14 -- local definition
   in 2 * pi * r -- <expr> will return a value

fact :: Integer -> Integer
fact n =
  let factHelp :: Integer -> Integer -> Integer
      -- maintains the result (tail recursive with pattern matching)
      factHelp 0 res = res
      factHelp currNum res = factHelp (currNum - 1) (currNum * res)
   in factHelp n 1

-- ghci> fact 3
-- 6

quad :: Int -> Int
quad x =
  let double :: Int -> Int
      double x = x * x
      num = 5
   in num + double x

-- ghci> quad  3 --> num + double x --> 5 + double 3 --> 5 + 9 --> 14
-- 14

-- let can be evaluated to an expr
-- (let g x = x + 1
--     y = 4
-- in g y) + 1

-- NOTICE we can use the locally declared variables interchangeably
payDistr :: Fractional d => d -> d -> (d, d, d, d)
payDistr hours rate =
  let gross = hours * rate
      ret = 0.05 * gross
      union = 0.03 * gross
      tax = 0.02 * (gross - (ret + union))
      net = gross - ret - union - tax
   in (net, ret, union, tax) -- tuple a.k.a multiple return

-----------------------------------------------------------------------------------------------------

-- <some definiton statement>
-- where
--   <any needed local declarations>

circ' :: Double -> Double
circ' r = 2 * pi * r -- <function definition>
  where
    pi = 3.14 -- <needed local declaration only visible in the scope of this function>

quad' :: Int -> Int
quad' x = num + double x
  where
    double :: Int -> Int
    double x = x * x
    num = 5

payDistr' :: Fractional d => d -> d -> (d, d, d, d)
payDistr' hours rate = (net, ret, union, tax) -- tuple a.k.a multiple return
  where
    gross = hours * rate
    ret = 0.05 * gross
    union = 0.03 * gross
    tax = 0.02 * (gross - (ret + union))
    net = gross - ret - union - tax

inRange' :: Int -> Int -> Int -> Bool
inRange' a b x = inLowerBound && inUpperBound -- <function definition>
  where
    -- <needed local declarations>
    inLowerBound = x >= a
    inUpperBound = x <= b

y :: Integer
y = 3 + num
  where
    num = 5

-- ghci> y
-- 8