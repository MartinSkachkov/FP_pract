main :: IO ()
main = do
  print $ areAmicable 200 300 == False
  print $ areAmicable 220 284 == True
  print $ areAmicable 284 220 == True
  print $ areAmicable 1184 1210 == True
  print $ areAmicable 2620 2924 == True
  print $ areAmicable 6232 6368 == True

areAmicable :: Int -> Int -> Bool
areAmicable x y = sum (divisors x) == y && sum (divisors y) == x

divisors :: Int -> [Int]
divisors n = [x | x <- [1 .. n - 1], x `divides` n]
  where
    divides :: Int -> Int -> Bool
    divides x n = n `mod` x == 0