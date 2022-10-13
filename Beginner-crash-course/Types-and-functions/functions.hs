in_range :: Int -> Int -> Int -> Bool
--in_range min max x =
--    x >= min && x <= max

--in_range 0.5 1.5 1 //error(floats)
--in_range 0 5 3 //OK(integers)

in_range min max x =
    let in_lower_bound = min <= x
        in_upper_bound = x <= max
    in
        (in_lower_bound && in_upper_bound)
---------------------------------------------------
fact :: Int -> Int
fact num =
    if (num == 0)
        then 1 --0!
        else num * fact (pred num)
---------------------------------------------------
fib :: Int -> Int
fib num =
    if (num == 0 || num == 1)
        then num
        else fib(num - 1) + fib(num - 2)
---------------------------------------------------
myPlus :: Int -> Int -> Int
myPlus x y =
    if (x == 0)
        then y --0 + y = y
        else succ(myPlus (pred x) y)
---------------------------------------------------
myMult :: Int -> Int -> Int
myMult x y =
    if (x == 0)
        then 0 --0 * y = 0
        else myPlus y (myMult (pred x) y)
---------------------------------------------------
divides :: Int -> Int -> Bool
divides num divisor =
    num `rem` divisor == 0
---------------------------------------------------
fastPow :: Int -> Int -> Int
fastPow base expon =
    if (expon == 0)
        then 1
        else
            if (expon `divides` 2) --it's if 2 divides expon but look at the args order in divides func
                then fastPow (base * base) (expon `div` 2)
                else base * fastPow base (pred expon)
 ---------------------------------------------------
isPrime :: Int -> Bool
isPrime num = not (anyDividesInRange num 2 (pred num))  

anyDividesInRange :: Int -> Int -> Int -> Bool
anyDividesInRange num start end =
    if (start > end)
        then False
        else num `divides` start || anyDividesInRange num (succ start) end